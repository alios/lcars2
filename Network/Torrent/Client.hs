{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module Network.Torrent.Client () where

import Prelude hiding (take, catch, FilePath)
import Filesystem.Path.CurrentOS
import Data.Attoparsec (Parser)       
import Data.Conduit.Attoparsec

import qualified Data.Attoparsec.Char8 as PC
import Data.Attoparsec
import qualified Codec.Binary.UTF8.Generic as UTF8

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



  
{-  

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
  
  



type PieceA = (SHA1, Int)

data PieceState = PieceUnknown 
                | PieceMissing 
                | PiecePartial Int
                | PieceOK
                deriving (Show, Eq)
                         
type PieceInfo = Map PieceA PieceState




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
-}          
        
