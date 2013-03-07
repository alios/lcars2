{-# LANGUAGE FlexibleContexts, ImpredicativeTypes #-}

module Data.Torrent.Conduit 
       ( sinkBencoded
       , sinkBdecoded
       , conduitBencoded
       , conduitBdecoded
       ) where

import Prelude hiding (FilePath, head)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Torrent.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Lazy
import Data.Conduit.Attoparsec
import Data.Conduit.Filesystem
import Filesystem.Path
import qualified Codec.Binary.UTF8.Generic as UTF8

sinkBencoded :: (MonadThrow m) => Consumer ByteString m BEncodedT
sinkBencoded = sinkParser parseBencoded

conduitBencoded :: (MonadThrow m) => Conduit ByteString m (PositionRange, BEncodedT)
conduitBencoded = conduitParser parseBencoded

sinkBdecoded :: (MonadThrow m) => Consumer BEncodedT m ByteString
sinkBdecoded = 
  await >>= (maybe sinkBdecoded (return . beEncodeByteString))

conduitBdecoded :: (MonadThrow m) => Conduit BEncodedT m ByteString
conduitBdecoded = awaitForever $ yield . beEncodeByteString 




{--

emptyTM :: TorrentMonad
emptyTM = (BDict [], [])


--foldTorrent :: (MonadIO m ) => Consumer FilePath m (TorrentMonad m)
--foldTorrent = foldM encodeTorrent emptyTM

encodeTorrent :: TorrentMonad -> FilePath -> m TorrentMonad
encodeTorrent torIn@(BDict [], []) fp = do 
  tor' <- addTracker torIn
  encodeTorrent (fst tor', []) fp
encodeTorrent (dict, fs) fp = do
  --          fileSrc <-  lazyConsume $ sourceFile fp
  return (dict,  fs)

addTracker :: TorrentMonad -> m TorrentMonad
addTracker (tor, fs) = do
  ann <- trackerSource $$ head 
  case (ann) of
    Nothing -> error $ "no tracker specified in " ++ show trackers
    Just ann' -> 
      return $ (BDict [ (mkBString "announce", mkBString ann') ], fs)


trackers :: [String]
trackers = []
trackerSource = sourceList trackers


--}



mkTorrent :: (MonadActive m, MonadBaseControl IO m, MonadResource m) => 
             FilePath -> [String] -> Int -> Producer m BEncodedT
mkTorrent fpBase tracker cSize = do
  
  -- add announce tracker to dict
  let trackerSource = sourceList tracker
  ann' <-  trackerSource $$ head
  let ann = maybe (error "no tracker specified") id ann'
  
  -- traverse files
--  let fileTravers = traverse False fpBase $$ foldFilePaths
--  let ffold = foldM  (BS.empty, [])
  
  --x <- fileTravers 
  
  yield $ BDict [(mkBString "announce", mkBString ann)]


foldFilePaths :: (Monad m, MonadActive m, MonadBaseControl IO m, MonadResource m) => 
                 Consumer FilePath m (ByteString, [(FilePath, Int)])
foldFilePaths = foldM f (BS.empty, []) 
  where f :: (Monad m, MonadActive m, MonadBaseControl IO m, MonadResource m) => 
             (ByteString, [(FilePath, Int)]) -> FilePath -> m (ByteString, [(FilePath, Int)])
        f (bs, fs) fp = do
          bs' <- fmap BS.concat (lazyConsume $ sourceFile fp)
          return (bs `BS.append` bs' , fs ++ [(fp, BS.length bs')])

