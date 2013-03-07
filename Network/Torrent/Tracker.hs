{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleContexts,  
             GADTs, Rank2Types #-}

module Network.Torrent.Tracker 
       (module Data.Torrent.TrackerTypes, app) where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Control
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
/announce AnnounceR GET
|]

instance Yesod Tracker
  
instance YesodPersist Tracker where
  type YesodPersistBackend Tracker = SqlPersist
  runDB action = do
    pool <- fmap tracker_pool getYesod
    runSqlPool action pool

newtype AnnounceError = AnnounceError String
                      deriving (Show)
instance Error AnnounceError where

getAnnounceR :: Handler RepPlain
getAnnounceR = undefined
  {-
type HandlerMonad = Either AnnounceError

f :: HandlerMonad RepPlain
f = h `catchError` Left

h :: HandlerMonad RepPlain
h  = undefined

g :: HandlerMonad RepPlain -> Handler RepPlain
g (Right rep) = do return $ RepPlain $ toContent $ show "foo"
g (Left e) = do return $ RepPlain $ toContent $ show "bar"
-}

  {-let inside = do  
        params <- getTrackerParams
      
        let uniq = 
              UniqueTorrentPeer 
              (torrentPeerInfo_hash params) 
              (torrentPeerPeer_id params)
        runDB $ do
          res <- getBy uniq
          case res of
            Nothing -> insert params
  
        liftIO $ print params
        return $ RepPlain $ toContent $ show "foo"
      outside = control inside
  in outside
-}

app cfg = 
  runResourceT $ withSqlitePool "tracker.db3" (configDBConnections cfg) $
  \pool -> do
    liftIO $ warpDebug (configPort cfg) $ Tracker cfg pool
