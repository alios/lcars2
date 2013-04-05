{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Network.Torrent.Client (StorageConfig(..), storage) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS

import Control.Monad
import Control.Exception as X
import Control.Monad.IO.Class
import qualified Control.Concurrent.Lifted as CC
import Data.Conduit
import Data.Conduit.Filesystem
import Data.Conduit.List  as CL

import Data.Torrent.MetaInfo
import Data.Torrent.Types
import Data.Torrent

data StorageConfig = StorageConfig {
  dataDir :: FilePath,
  torrentDir :: FilePath
  } deriving (Eq, Show) 

data StorageState = StorageSt {
  storageCfg :: StorageConfig
  } deriving (Eq, Show)


tryReadTorrent :: (MonadResource m) => Conduit FilePath m BEncodedT
tryReadTorrent = awaitForever $ \fp -> do
  t <- runExceptionT $ (sourceFile fp $$ sinkBencoded)
  case t of 
    Left _ -> tryReadTorrent
    Right t' -> yield t'
  tryReadTorrent


t :: IO ()
t = do
  t <- runResourceT $ st $$ await
  print t
  where baseDir = "/home/alios/tmp/torrent"
        st = storage (StorageConfig (baseDir </> "d/")(baseDir </> "t/"))
        
        
storage :: MonadResource m => 
           StorageConfig -> Source m StorageState
storage cfg = bracketP storageInit storageRelease storageMain
  where storageInit :: IO StorageState
        storageInit = do
          liftIO . print $ "storageInit with: " ++ show cfg
          tfs <- runResourceT $ readTorrentsConduit
          tpvs <- liftIO $ Prelude.sequence [metaInit t $ dataDir cfg | t <- tfs]
          return $ StorageSt {
            storageCfg = cfg
            }
        readTorrentsConduit =        
          (traverse True $ torrentDir cfg) 
          $= CL.filter (\fp -> fp `hasExtension` "torrent")
          $= tryReadTorrent
          $$ consume
  
          
storageRelease :: StorageState -> IO ()
storageRelease st = do
  liftIO . print $ "storageRelease with: " ++ show st

  
storageMain :: (MonadIO m) => StorageState -> Source m StorageState
storageMain st = do
    liftIO . print $ "storageMain with: " ++ show st
    yield st
    storageMain st






{-
initStorage :: (MonadIO m, MetaInfo meta) => 
               StorageConfig -> Maybe [meta] -> m ()
initStorage cfg ts = do
  fpProd = map (
      
  return ()
  
-}




        
