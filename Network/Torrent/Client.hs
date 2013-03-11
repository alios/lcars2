{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module Network.Torrent.Client (TorrentClient(..)) where

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
import Data.Torrent
import Data.Conduit
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (IOException, catch)
import qualified Data.Conduit.Network as CN



data PieceState = Piece

data ClientState = ClientState {
  csTorrent :: (MetaInfo t) => t
  }

class TorrentClient c where
  
  
handShakeText :: String
handShakeText = "BitTorrent protocol"

parseTorrentMagic = do
  hs <- parseW8RunLength
  if (hs /= handShakeText)
    then fail $ "handshake is not '" ++ handShakeText 
         ++ "'" ++  " but '" ++ hs ++ "'"  
    else return ()

parseTorrentHandshake :: Parser (SHA1, ByteString, ByteString)
parseTorrentHandshake = do
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
  
  return $ (h, peerid, reservedBytes)
    

parseW8RunLength :: Parser String
parseW8RunLength = do
  l <- fmap (fromInteger . toInteger) PB.anyWord8
  fmap UTF8.toString $ PB.take l


minClientPort = 6881
maxClientPort = 6889

clientServer :: (MetaInfo t) => Maybe Int -> [t] -> FilePath -> IO ()
clientServer Nothing ts datadir = clientServer (Just minClientPort) ts datadir
clientServer (Just port) ts datadir = 
  catch (clientServer' port ts datadir) $ 
  \(_ :: IOException) -> if (port >= maxClientPort) 
                         then clientServer Nothing ts datadir
                         else clientServer (Just $ port + 1) ts datadir



clientServer' :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MetaInfo t) => 
                  Int -> [t] -> FilePath ->  m ()
clientServer' port ts datadir = CN.runTCPServer cfg serverApp 
  where cfg = CN.serverSettings port CN.HostAny
        serverApp app = do
          (ihash, peerid, reserved) <- CN.appSource app $$ 
                                       sinkParser parseTorrentHandshake 
          
          return ()
          
