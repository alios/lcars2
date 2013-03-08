{-# LANGUAGE FlexibleContexts #-}

module Data.Torrent.Conduit 
       ( sinkBencoded
       , sinkBdecoded
       , conduitBencoded
       , conduitBdecoded
       , mkTorrent
       , mkTorrent'
       , defaultChunkSize
       , hashChunks
       ) where

import qualified Prelude as P
import Prelude hiding (FilePath, head)
import Filesystem.Path.CurrentOS
import qualified Data.List as List (map)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadActive, liftResourceT)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Filesystem as CFS
import Data.Conduit.Attoparsec
import Crypto.Conduit (sinkHash)
import Crypto.Hash.SHA1
import qualified Crypto.Classes as Crypto
import System.Posix.Files (isDirectory, getFileStatus)



import Data.Torrent.Types

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
             FilePath -> [String] -> Integer -> m (Maybe BEncodedT)  
mkTorrent fp ts cSize = 
  runResourceT $ liftResourceT $ 
  mkTorrent' fp ts cSize $$ await

mkTorrent' :: (MonadActive m, MonadBaseControl IO m, MonadResource m) => 
             FilePath -> [String] -> Integer -> Producer m BEncodedT
mkTorrent' fpBase tracker cSize = do  
  -- file or directory?
  isDir <- fmap isDirectory $ liftIO $ 
           getFileStatus $ encodeString fpBase
  
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
        (mkBString "name", mkBString . encodeString . filename $ fpBase),
        (mkBString "piece length", BInteger . toInteger $ cSize),
        (mkBString "pieces", BString $ pieces)
        ] ++ if ((length fs) == 1) 
             then [(mkBString "length", BInteger . toInteger . BL.length $ bs')]
             else [(mkBString "files", 
                    BList [ BDict [(mkBString "length", BInteger $ toInteger l), 
                                   (mkBString "path", mkBString . encodeString $ fn )] 
                          | (fn,l) <- fs])]
  
  yield $ BDict [
    (mkBString "announce", mkBString ann),
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

  