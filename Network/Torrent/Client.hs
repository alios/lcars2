{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Network.Torrent.Client () where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS

import Control.Monad.IO.Class
import qualified Control.Concurrent.Lifted as CC
import Data.Conduit
import Data.Conduit.Filesystem
import Data.Conduit.List  as CL

import Data.Torrent.MetaInfo
import Data.Torrent.Types
import Data.Torrent.Conduit

data StorageConfig = StorageConfig {
  dataDir :: FilePath,
  torrentDir :: FilePath
  }

data StorageState = StorageSt {
  storageCfg :: StorageConfig
  }


tryReadTorrent :: (MonadResource m) => Conduit FilePath m BEncodedT
tryReadTorrent = awaitForever $ \fp -> do
  t <- sourceFile fp $$ sinkBencoded
  yield t
  tryReadTorrent

storage :: MonadResource m => StorageConfig -> ConduitM () () m ()
storage cfg = bracketP storageInit storageRelease storageMain
  where storageInit :: IO StorageState
        storageInit = do
          tfs <- runResourceT $ 
                 (traverse True $ torrentDir cfg) 
                 $= CL.filter (\fp -> fp `hasExtension` "torrent")
                 $= tryReadTorrent
                 $$ consume
          
          return $ StorageSt {
            storageCfg = cfg
            }
          
          
storageRelease :: StorageState -> IO ()
storageRelease = do
  undefined
storageMain :: StorageState -> Conduit () m ()
storageMain st = do
  undefined

{-
initStorage :: (MonadIO m, MetaInfo meta) => 
               StorageConfig -> Maybe [meta] -> m ()
initStorage cfg ts = do
  fpProd = map (
      
  return ()
  
-}




        
