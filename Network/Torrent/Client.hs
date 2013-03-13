{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module Network.Torrent.Client (PeerMessage(..), clientServer) where

import Prelude hiding (take, catch, FilePath)
import Filesystem.Path.CurrentOS
import Data.Attoparsec (Parser)       
import Data.Conduit.Attoparsec
import qualified Data.Attoparsec.ByteString as PB
import qualified Data.Attoparsec.Char8 as PC
import Data.Attoparsec
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Data.Serialize as Ser
import Crypto.Hash.SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Torrent
import Data.Conduit
import Data.Conduit.Bits
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (IOException, catch)
import qualified Data.Conduit.Network as CN
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import Data.Maybe
import Crypto.Random.API
import Data.Conduit.TMChan



  
randomPeerId :: IO ByteString  
randomPeerId = fmap (fst. genRandomBytes 20) getSystemRandomGen
  
handShakeText :: String
handShakeText = "BitTorrent protocol"

parseTorrentMagic = do
  hs <- parseW8RunLength
  if (hs /= handShakeText)
    then fail $ "handshake is not '" ++ handShakeText 
         ++ "'" ++  " but '" ++ hs ++ "'"  
    else return ()


data ClientHandshake = ClientHandshake {
  hsInfoHash :: SHA1,
  hsPeerId :: ByteString,
  hsReservedBytes :: ByteString
  }

  

i2w8 :: Int -> Word8
i2w8 i =
  let max' = fromInteger . toInteger $ (maxBound :: Word8)
      min' = fromInteger . toInteger $ (minBound :: Word8)
  in if (i < min' || i > max') 
     then error $ show i ++ " is out of bounds for Word8 conversion."
     else fromInteger.toInteger $ i

conduitClientHandshake :: (Monad m) => Conduit ClientHandshake m ByteString
conduitClientHandshake = do
  hs' <- await
  yield . BS.singleton . i2w8 . length $ handShakeText
  yield . UTF8.fromString $ handShakeText
  case (hs') of
    Nothing -> return ()
    Just hs -> do
      yield . hsReservedBytes $ hs
      yield . Ser.encode . hsInfoHash $ hs
      yield . hsPeerId $ hs
  
  
sinkClientHandshake :: (MonadThrow m) => Consumer ByteString m ClientHandshake
sinkClientHandshake = sinkParser parseClientHandshake 

parseClientHandshake :: Parser ClientHandshake
parseClientHandshake = do
  -- torrent magic
  try $ parseTorrentMagic
  
  -- reserved bytes
  reservedBytes <- take 8
  
  -- info hash
  ih' <- fmap Ser.decode $ PB.take 20
  h <- case (ih') of
    Left err -> fail $ "unable to decode SHA1 hash: " ++ err
    Right h -> return h
  
  -- peer id 
  peerid <- PB.take 20
  return $ ClientHandshake h peerid reservedBytes
    

parseW8RunLength :: Parser String
parseW8RunLength = do
  l <- fmap (fromInteger . toInteger) PB.anyWord8
  fmap UTF8.toString $ PB.take l


type PieceA = (SHA1, Int)

data PieceState = PieceUnknown 
                | PieceMissing 
                | PiecePartial Int
                | PieceOK
                deriving (Show, Eq)
                         
type PieceInfo = Map PieceA PieceState

-- | After the client protocol handshake, there comes an alternating stream of length prefixes and 'PeerMessage's
data PeerMessage =
  -- | Messages of length zero are keepalives, and ignored. Keepalives are generally sent once every two minutes, but note that timeouts can be done much more quickly when data is expected.
  KeepAlive |
  Choke |
  Unchoke |
  Interested |
  NotInterested |
  -- | The 'Have' message's payload is a single number, the index which that downloader just completed and checked the hash of.
  Have Int |
  -- | 'Bitfield' is only ever sent as the first message. Its payload is a bitfield with each index that downloader has sent set to one and the rest set to zero. Downloaders which don't have anything yet may skip the 'bitfield' message. The first byte of the bitfield corresponds to indices 0 - 7 from high bit to low bit, respectively. The next one 8-15, etc. Spare bits at the end are set to zero.
  Bitfield ByteString |
  -- | 'Request' messages contain an index, begin, and length. The last two are byte offsets. Length is generally a power of two unless it gets truncated by the end of the file. All current implementations use 2 15 , and close connections which request an amount greater than 2 17.
  Request Int Int Int |
  -- | 'Piece' messages contain an index, begin, and piece. Note that they are correlated with request messages implicitly. It's possible for an unexpected piece to arrive if choke and unchoke messages are sent in quick succession and/or transfer is going very slowly.
  Piece Int Int ByteString |
  -- | 'Cancel' messages have the same payload as request messages. They are generally only sent towards the end of a download, during what's called 'endgame mode'. When a download is almost complete, there's a tendency for the last few pieces to all be downloaded off a single hosed modem line, taking a very long time. To make sure the last few pieces come in quickly, once requests for all pieces a given downloader doesn't have yet are currently pending, it sends requests for everything to everyone it's downloading from. To keep this from becoming horribly inefficient, it sends cancels to everyone else every time a piece arrives.
  Cancel Int Int Int
  deriving (Show, Eq)



minClientPort = 6881
maxClientPort = 6889


torrentMap :: (Monad m, MonadThrow m, MetaInfo t) => [t] -> m (Map SHA1 t)
torrentMap ts = do
  ihs <- sequence $ map infoHash ts
  return $ Map.fromList $ zip ihs ts

-- | The 'clientServer' takes an optional port number to listen on, a list of 'MetaInfo' files and a data dir.
clientServer :: (MetaInfo t) => Maybe Int -> [t] -> FilePath -> IO ()
clientServer Nothing ts datadir = do 
  clientServer (Just minClientPort) ts datadir
clientServer (Just port) ts' datadir = do
  ts <- torrentMap ts'
  catch (clientServer' port ts datadir) $ 
    \(_ :: IOException) -> if (port >= maxClientPort) 
                           then clientServer Nothing ts' datadir
                           else clientServer (Just $ port + 1) ts' datadir



clientServer' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MetaInfo t) => 
                  Int -> Map SHA1 t -> FilePath ->  m ()
clientServer' port ts datadir = CN.runTCPServer cfg serverApp 
  where cfg = CN.serverSettings port CN.HostAny
        serverApp app = do
          -- random peer id
          mypeerid <- liftIO randomPeerId
          
          -- initialize all torrents
          
          -- fork worker clients (per torrent)
          
          -- wait for handshake
          hs <- CN.appSource app $$ sinkClientHandshake
          let ihash = hsInfoHash hs
          
          -- lookup the torrent in local torrent list by info hash
          let mytorrent' = Map.lookup ihash ts
          mytorrent <- case (mytorrent') of
            Nothing -> 
              fail $ "client requested unknown torrent: " ++ show ihash
            Just t -> return t
          
          -- answer with my Handshake
          if (mypeerid == hsPeerId hs)
               then fail $ "client send our peer id: " ++ show mypeerid
               else do  
                 let hs = ClientHandshake ihash mypeerid 
                          (BS.pack $ replicate 8 0x0)
                 yield hs $= conduitClientHandshake $$ CN.appSink app
          
        
