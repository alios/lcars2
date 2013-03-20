{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module Data.Torrent.Conduit 
       ( sinkBencoded
       , sinkBdecoded
       , conduitBencoded
       , conduitBdecoded
       , mkTorrent
       , mkTorrent'
       , defaultChunkSize
       , hashChunks
       , conduitPeerMessage
       , conduitDePeerMessage
       , peerConnect
       , peerListen
       ) where


import Prelude hiding (FilePath, head, catch)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Filesystem.Path.CurrentOS
import qualified Data.List as List (map)

import Network.Socket (SockAddr)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadActive, liftResourceT)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Filesystem as CFS

import Data.Conduit.Attoparsec
import qualified Data.Attoparsec.ByteString as PB

import Crypto.Conduit (sinkHash)
import Crypto.Hash.SHA1
import qualified Crypto.Classes as Crypto

import System.Posix.Files (isDirectory, getFileStatus)
import Network.URI

import Data.Torrent.Types
import Data.Torrent.MetaInfo

import qualified Data.Serialize as Ser

data PeerAppData i m = PeerAppData { 
  peerappInfo :: (MetaInfo i) => i,
  peerappMyId :: SHA1,
  peerappPeerId :: SHA1,
  peerappSink :: Consumer PeerMessage m (),
  peerappSource :: Producer m PeerMessage,
  peerappSockAddr :: SockAddr,
  peerappLocalAddr :: Maybe SockAddr
  }
                     

peerListen :: (MetaInfo i, MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
              CN.ServerSettings m -> Maybe SHA1 -> Map SHA1 i -> m ()
peerListen s p is = CN.runTCPServer s $ peerListen' p is s

peerListen' :: (MetaInfo i, MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
               Maybe SHA1 -> Map SHA1 i -> CN.ServerSettings m -> CN.Application m
peerListen' p' is s server = do
  -- peer id given?
  p <- case p' of
    Just pp -> return pp
    Nothing -> undefined
  
  -- receive handshake from peer
  phs <- CN.appSource server $$ sinkClientHandshake
  
  
  let peerid = either error id $ Ser.decode $ hsPeerId phs
      cmSink = toConsumer $ conduitDePeerMessage =$ CN.appSink server
      cmSource = toProducer $ CN.appSource server $= conduitPeerMessage
      i' = Map.lookup (hsInfoHash phs) is
  
  case i' of
    Nothing -> return ()
    Just i -> do 
      let appData = PeerAppData {
            peerappInfo = i,
            peerappPeerId = peerid,
            peerappMyId = p,
            peerappSink = cmSink,
            peerappSource = cmSource,
            peerappSockAddr = CN.appSockAddr server,
            peerappLocalAddr = CN.appLocalAddr server
            }
                 
      -- send handshake to peer
      ih <- infoHash i
      let hs = ClientHandshake {
            hsInfoHash = ih,
            hsPeerId = Ser.encode p,
            hsReservedBytes = BS.pack $ replicate 8 0x00
            }
      yield hs $= conduitClientHandshake $$ CN.appSink server
      peerApp appData


peerConnect :: (MetaInfo i, MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
              CN.ClientSettings m -> Maybe SHA1 -> i -> m ()
peerConnect s p is = CN.runTCPClient s $ peerConnect' p is s

peerConnect' :: (MetaInfo i, MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
                Maybe SHA1 -> i -> CN.ClientSettings m -> CN.Application m
peerConnect' p' i s client = do
  -- peer id given?
  p <- case p' of
    Just pp -> return pp
    Nothing -> undefined
    
  -- calculate info hash
  ih <- infoHash i
           
  -- send handshake to peer
  let hs = ClientHandshake {
        hsInfoHash = ih,
        hsPeerId = Ser.encode p,
        hsReservedBytes = BS.pack $ replicate 8 0x00
        }
  yield hs $= conduitClientHandshake $$ CN.appSink client
  
  -- receive handshake from peer
  phs <- CN.appSource client $$ sinkClientHandshake
  let peerid = either error id $ Ser.decode $ hsPeerId phs
      
  -- abort if unknown infohash or if remote side has same client id 
  if (hsInfoHash phs /= ih || peerid ==p )
    then return ()
    else do
    let cmSink = toConsumer $ conduitDePeerMessage =$ CN.appSink client
        cmSource = toProducer $ CN.appSource client $= conduitPeerMessage            
        appData = PeerAppData {
          peerappInfo = i,
          peerappPeerId = peerid,
          peerappMyId = p,
          peerappSink = cmSink,
          peerappSource = cmSource,
          peerappSockAddr = CN.appSockAddr client,
          peerappLocalAddr = CN.appLocalAddr client
          }
    peerApp appData

type PeerApp m i = Monad m => PeerAppData i m -> m ()

peerApp :: PeerApp m i
peerApp app = do
  
  
  peerApp app
  
  
  

sinkClientHandshake :: (MonadThrow m) => Consumer ByteString m ClientHandshake
sinkClientHandshake = sinkParser parseClientHandshake 

conduitClientHandshake :: Conduit ClientHandshake m ByteString
conduitClientHandshake = undefined


conduitPeerMessage :: (MonadThrow m) => Conduit ByteString m PeerMessage
conduitPeerMessage = do
  l' <- CB.take 4
  let l = runGet getBInt l'
  bs' <- CB.take l
  let bs = BL.fromChunks $ BL.toChunks l' ++ BL.toChunks bs'
  yield $ runGet (get :: Get PeerMessage) bs
  conduitPeerMessage
      
conduitDePeerMessage :: (MonadThrow m) => Conduit PeerMessage m ByteString
conduitDePeerMessage = do
  msg' <- await
  case msg' of 
    Nothing -> conduitDePeerMessage
    Just msg -> do
      yield . BS.concat . BL.toChunks . runPut $ put msg
      conduitDePeerMessage
  
sinkBencoded :: (MonadThrow m) => Consumer ByteString m BEncodedT
sinkBencoded = sinkParser parseBencoded

conduitBencoded :: (MonadThrow m) => Conduit ByteString m (PositionRange, BEncodedT)
conduitBencoded = conduitParser parseBencoded

sinkBdecoded :: (MonadThrow m) => Consumer BEncodedT m ByteString
sinkBdecoded = 
  await >>= (maybe sinkBdecoded (return . beEncodeByteString))

conduitBdecoded :: (MonadThrow m) => Conduit BEncodedT m ByteString
conduitBdecoded = awaitForever $ yield . beEncodeByteString 

defaultChunkSize :: Integer
defaultChunkSize = 2 ^ 18

mkTorrent :: (MonadIO m, MonadUnsafeIO m, MonadThrow m, MonadBaseControl IO m) =>
             FilePath -> [URI] -> Integer -> m (Maybe BEncodedT)  
mkTorrent fp ts cSize = 
  runResourceT $ liftResourceT $ 
  mkTorrent' fp ts cSize $$ await

mkTorrent' :: (MonadActive m, MonadBaseControl IO m, MonadResource m) => 
             FilePath -> [URI] -> Integer -> Producer m BEncodedT
mkTorrent' fpBase' tracker cSize = do  
  -- file or directory?
  isDir <- fmap isDirectory $ liftIO $ 
           getFileStatus $ encodeString fpBase'
  let (fpBase, tname) =
        if isDir 
        then (fpBase' </> empty, dirname fpBase)
        else (fpBase', filename fpBase)
  -- traverse files
  (bs', fs) <- if (isDir) 
               then liftIO $ runResourceT $ 
                    CFS.traverse False fpBase $$ foldFilePaths fpBase
               else do
                 bs' <- fmap BL.fromChunks $ CFS.sourceFile fpBase $$ CL.consume
                 return (bs', [(fpBase, toInteger . BL.length $ bs')])
  
  -- hash pieces 
  pieces' <- (CB.sourceLbs $ bs') $= hashChunks (fromInteger cSize) $$ CL.consume
  let pieces = BS.concat $ List.map Crypto.encode pieces'
  
  -- add announce tracker to dict
  ann' <- CL.sourceList tracker $$ CL.head
  let ann = maybe (error "no tracker specified") id ann'
  
  let info = BDict $ [
        (mkBString "name", mkBString . encodeString $ tname),
        (mkBString "piece length", BInteger . toInteger $ cSize),
        (mkBString "pieces", BString $ pieces)
        ] ++ if ((length fs) == 1) 
             then [(mkBString "length", BInteger . toInteger . BL.length $ bs')]
             else [(mkBString "files", 
                    BList [ BDict [(mkBString "length", BInteger $ toInteger l), 
                                   (mkBString "path", mkBString . encodeString $ fn )] 
                          | (fn,l) <- fs])]
  
  yield $ BDict [
    (mkBString "announce", mkBString $ show ann),
    (mkBString "info", info)
    ]



foldFilePaths :: (Monad m, MonadActive m, MonadBaseControl IO m, MonadResource m) => 
                 FilePath -> Consumer FilePath m (BL.ByteString, [(FilePath, Integer)])
foldFilePaths fpBase = CL.foldM f (BL.empty, []) 
  where f :: (Monad m, MonadActive m, MonadBaseControl IO m, MonadResource m) => 
             (BL.ByteString, [(FilePath, Integer)]) -> FilePath -> m (BL.ByteString, [(FilePath, Integer)])
        f (bs, fs) fp = do
          bs' <- fmap BS.concat (CFS.sourceFile fp $$ CL.consume)
          return (bs `BL.append` BL.fromChunks [bs'] , fs ++ [(fromJust $ stripPrefix fpBase fp, toInteger $ BS.length bs')])

hashChunks :: (Monad m) => Int -> Conduit ByteString m SHA1
hashChunks cSize = do
  chunk <- CB.take cSize
  h <- CB.sourceLbs chunk $$ sinkHash
  yield h
  next <- CL.peek
  case next of
    Nothing -> return ()
    Just _ -> hashChunks cSize



