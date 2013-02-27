{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleContexts,  
             GADTs #-}

module Network.Torrent.Tracker 
       (module Data.Torrent.TrackerTypes, app) where

import Data.Torrent
import Data.Torrent.TrackerTypes
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Yesod


data Tracker = Tracker {
  tracker_config :: TrackerConfig,
  tracker_pool :: ConnectionPool
  }

mkYesod "Tracker" [parseRoutes|
/tracker HomeR GET
|]

instance Yesod Tracker
  
instance YesodPersist Tracker where
  type YesodPersistBackend Tracker = SqlPersist
  runDB action = do
    pool <- fmap tracker_pool getYesod
    runSqlPool action pool

getHomeR :: Handler RepPlain
getHomeR = do  
  params <- getTrackerParams
  let uniq = UniqueTorrentPeer 
             (torrentPeerInfo_hash params) 
             (torrentPeerPeer_id params)
  runDB $ do
    res <- getBy uniq
    case res of
      Nothing -> insert params
      
  liftIO $ print params
  return $ RepPlain $ toContent $ show "foo"



app cfg = 
  runResourceT $ withSqlitePool "tracker.db3" (configDBConnections cfg) $
  \pool -> do
    liftIO $ warpDebug (configPort cfg) $ Tracker cfg pool
