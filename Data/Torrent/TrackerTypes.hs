{-# LANGUAGE TypeFamilies, QuasiQuotes,
             TemplateHaskell, OverloadedStrings, FlexibleContexts,  
             GADTs, DeriveDataTypeable #-}

module Data.Torrent.TrackerTypes where

import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Int
import Data.Time
import Database.Persist
import Database.Persist.TH
import qualified Data.Map as Map
import Data.Time
import Data.Maybe (fromJust)
import qualified Codec.Binary.UTF8.Generic as UTF8
import Network.Wai
import Yesod

data TrackerConfig = 
  TrackerConfig {
    configDBConnections :: Int,
    configPort :: Int
    } deriving (Show, Eq, Data, Typeable)
               
defaultTrackerConfig :: TrackerConfig
defaultTrackerConfig = TrackerConfig {
  configDBConnections = 10,
  configPort = 3000
  }

data TrackerEvent = 
  Started | Completed | Stoped | Empty
  deriving (Show, Read, Eq, Enum, Data, Typeable)
derivePersistField "TrackerEvent"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
TorrentPeer
  info_hash ByteString
  peer_id ByteString
  ip String
  port String
  uploaded Int64
  downloaded Int64
  left Int64
  event TrackerEvent
  created UTCTime default=CURRENT_TIME
  UniqueTorrentPeer info_hash peer_id
  deriving Show
|]

getTrackerParams :: GHandler sub master TorrentPeer
getTrackerParams = do
  qs <- fmap (Map.fromList . queryString) waiRequest
  t <- liftIO $ getCurrentTime

  let q s = case (Map.lookup (UTF8.fromString s) qs) of
        Nothing -> error $ "no GET parameter " ++ s
        Just r -> fromJust r

  return $ TorrentPeer { 
    torrentPeerInfo_hash = q "info_hash",
    torrentPeerPeer_id = q "info_hash",
    torrentPeerIp = UTF8.toString  $ q "ip",
    torrentPeerPort = UTF8.toString $ q "port",
    torrentPeerUploaded = read . UTF8.toString $ q "uploaded",
    torrentPeerDownloaded = read . UTF8.toString $ q "downloaded",
    torrentPeerLeft = read . UTF8.toString $ q "left",
    torrentPeerEvent = readEvent . UTF8.toString $ q "event",
    torrentPeerCreated = t
    }
  where 
    readEvent "started" = Started
    readEvent "completed" = Completed
    readEvent "stoped" = Stoped
    readEvent _ = Empty
