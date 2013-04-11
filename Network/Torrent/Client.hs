{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Network.Torrent.Client (
  StorageConfig(..),
  StorageCmd(..),
  StorageEvent(..),
  StoragePiece,
  PieceInfo,
  storage) where

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

import Data.ByteString (ByteString)
import Control.Monad.STM
import Data.Conduit.TMChan
import Control.Concurrent (forkIO, killThread)

data StorageConfig = StorageConfig {
  dataDir :: FilePath,
  torrentDir :: FilePath
  } deriving (Eq, Show) 

data StorageState = StorageSt {
  storageCfg :: StorageConfig
  } deriving (Eq, Show)


type PieceInfo = (SHA1, Integer)
type StoragePiece = (PieceInfo, ByteString)

data StorageCmd   = 
  StorageCmdPiece !StoragePiece |
  StorageCmdShutdown

data StorageEvent = 
  StorageNewPiece !PieceInfo |
  StorageShutdown
  
tryReadTorrent :: (MonadResource m) => Conduit FilePath m BEncodedT
tryReadTorrent = awaitForever $ \fp -> do
  t <- runExceptionT $ (sourceFile fp $$ sinkBencoded)
  case t of 
    Left _ -> tryReadTorrent
    Right t' -> yield t'
  tryReadTorrent


t :: IO ()
t = do
  _ <- runExceptionT . runResourceT $ st $$ await
  _ <- runExceptionT . runResourceT $ st $$ await
  _ <- runExceptionT . runResourceT $ st $$ await
  _ <- runExceptionT . runResourceT $ st $$ await
  
  return ()
  where baseDir = "/home/alios/tmp/torrent"
        st = storage (StorageConfig (baseDir </> "d/")(baseDir </> "t/"))

storage :: MonadResource m => StorageConfig -> Source m ()
storage cfg = bracketP (storageThreadInit cfg) storageThreadRelease storageThreadMain
  where storageThreadInit cfg =
          let mkChans = atomically $ do 
                cmdChan <- newTBMChan 16
                eventChan <- newTBMChan 16
                return (cmdChan, eventChan)
              storageThread (cmdChan, eventChan) = do
                r <- runExceptionT . runResourceT $ 
                     sourceTBMChan cmdChan =$= storage' cfg $$ sinkTBMChan eventChan
                case r of
                  Left err -> fail $ show err
                  Right () -> return ()
          in do              
            liftIO . print $ "storageThreadInit with: " ++ show cfg
            chans <- mkChans
            tid <- forkIO $ storageThread chans
            return (tid, chans)
        storageThreadRelease (tid, (cmdChan, eventChan)) = do
          liftIO . print $ "storageThreadRelease with TID: " ++ show tid
          cl <- atomically . isClosedTBMChan $ cmdChan
          if (not cl)
            then do
            -- sent shutdown
            atomically $ unGetTBMChan cmdChan StorageCmdShutdown
             -- wait for shutdown notice
            let waitForShutdown = do
                  e <- (sourceTBMChan eventChan) $$ await
                  case e of
                    Just StorageShutdown -> return ()
                    _ -> waitForShutdown
            runResourceT waitForShutdown
            else return ()
          killThread tid
        storageThreadMain args@(tid, chans) = do
          liftIO . print $ "storageThreadMain"
          yield ()
          
          storageThreadMain args
          

storage' :: MonadResource m => 
           StorageConfig -> Conduit StorageCmd m StorageEvent
storage' cfg = stConduit
  where stConduit =  bracketP storageInit storageRelease storageMain
        storageInit :: IO StorageState
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

  
storageMain :: (MonadIO m) => StorageState -> Conduit StorageCmd m StorageEvent
storageMain st = awaitForever $ \cmd -> do
  liftIO . print $ "storageMain with: " ++ show st
  case cmd of
    StorageCmdShutdown -> return ()
    StorageCmdPiece ((ihash, pid), bs) -> do
      storageMain st






{-
initStorage :: (MonadIO m, MetaInfo meta) => 
               StorageConfig -> Maybe [meta] -> m ()
initStorage cfg ts = do
  fpProd = map (
      
  return ()
  
-}




        
