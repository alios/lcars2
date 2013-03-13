{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Torrent 
       ( module Data.Torrent.Conduit
       , MetaInfo(..)
       ) where

import Data.Maybe (fromJust)
import Data.Torrent.Types
import Data.Torrent.Conduit
import Crypto.Hash.SHA1

import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Binary 
import Crypto.Conduit

-- | Metainfo files (also known as .torrent files) are bencoded dictionaries
class MetaInfo t where
  -- | This maps to a dictionary, with keys described below.
  metaInfo :: t -> Map String BEncodedT
  -- | The URL of the tracker.
  announce :: t -> String
  announce = beStringUTF8 . fromJust . Map.lookup "announce" . metaInfo
  -- | This maps to a dictionary, with keys described below.
  info :: t -> Map String BEncodedT
  -- | The 20 byte sha1 hash of the bencoded form of the info value from the metainfo file.
  infoHash :: t -> (Monad m, MonadThrow m) => m SHA1
  -- | The name key maps to a UTF-8 encoded string which is the suggested name to save the file (or directory) as. It is purely advisory.
  infoName :: t -> String
  -- | Maps to a UTF-8 encoded string which is the suggested name to save the file (or directory) as. It is purely advisory.
  infoName = beStringUTF8 . fromJust . Map.lookup "name" . info 
  -- | Maps to the number of bytes in each piece the file is split into. For the purposes of transfer, files are split into fixed-size pieces which are all the same length except for possibly the last one which may be truncated. piece length is almost always a power of two, most commonly 2 18 = 256 K (BitTorrent prior to version 3.2 uses 2 20 = 1 M as default).
  infoPieceLength :: t -> Integer
  infoPieceLength = beInteger . fromJust . Map.lookup "piece length" . info 
  -- | Maps to a string whose length is a multiple of 20. It is to be subdivided into strings of length 20, each of which is the SHA1 hash of the piece at the corresponding index.
  infoPieces :: t -> [ByteString]
  infoPieces mi = 
    let f bs 
          | (bs == BS.empty) = []
          | (BS.length bs >= 20) = (BS.take 20 bs) : f (BS.drop 20 bs)
          | otherwise = error $ "found invalid piece of length: " ++ show (BS.length bs)
    in f $ beString' . fromJust . Map.lookup "pieces" $ info mi
  -- |  If length is present then the download represents a single file, otherwise it represents a set of files which go in a directory structure.
  infoLength :: t -> Maybe Integer
  infoLength m = case (Map.lookup "length" $ info m) of
    Nothing -> Nothing
    Just d -> Just $ beInteger d
  infoFiles :: t -> Maybe (Map String BEncodedT)
  -- | For the purposes of the other keys, the multi-file case is treated as only having a single file by concatenating the files in the order they appear in the files list. The files list is the value files maps to, and is a list of dictionaries containing the following keys: 'length', 'path'.
  infoFiles m = case (Map.lookup "files" $ info m) of
    Nothing -> Nothing
    Just d -> Just $ beDictUTF8 d
  infoLengthFiles :: t -> Either Integer (Map String BEncodedT)
  infoLengthFiles m = 
    let l  = infoLength m
        fs = infoFiles m
        i = info m
        lfs = (l,fs)
    in case lfs of
      (Nothing, Nothing) -> error $ "unable to lookup either 'length' or 'files' in: " ++ show i 
      (Just l', Nothing) -> Left l'
      (Nothing, Just fs') -> Right fs'
      (Just _, Just _) -> error $ "error, both 'length' and 'files' in: " ++ show i

instance MetaInfo BEncodedT where
  metaInfo = beDictUTF8
  info m = case (beDict "info" m) of
    Nothing -> error $ "unable to lookup info dict"
    Just i -> beDictUTF8 i
  infoHash m = case (beDict "info" m) of
    Nothing -> fail $ "unable to lookup info dict"
    Just i -> yield i $= conduitBdecoded $$ sinkHash
    


propSize :: MetaInfo t => t -> Bool
propSize t =
  let c =  length (infoPieces t)
      pl = infoPieceLength t
      l' = toInteger c * pl
      l = infoLength t
  in case (l) of
    Nothing -> undefined
    Just l'' -> l'' == l'
