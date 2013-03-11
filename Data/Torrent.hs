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

class MetaInfo t where
  metaInfo :: t -> Map String BEncodedT
  announce :: t -> String
  announce = beStringUTF8 . fromJust . Map.lookup "name" . metaInfo
  info :: t -> Map String BEncodedT
  infoHash :: t -> IO SHA1
  infoName :: t -> String
  infoName = beStringUTF8 . fromJust . Map.lookup "info" . info 
  infoPieceLength :: t -> Integer
  infoPieceLength = beInteger . fromJust . Map.lookup "piece length" . info 
  infoPieces :: t -> [ByteString]
  infoPieces mi = 
    let f bs 
          | (bs == BS.empty) = []
          | (BS.length bs >= 20) = (BS.take 20 bs) : f (BS.drop 20 bs)
          | otherwise = error $ "found invalid piece of length: " ++ show (BS.length bs)
    in f $ beString' . fromJust . Map.lookup "pieces" $ info mi
         
  infoLength :: t -> Maybe Integer
  infoLength m = case (Map.lookup "length" $ info m) of
    Nothing -> Nothing
    Just d -> Just $ beInteger d
  infoFiles :: t -> Maybe (Map String BEncodedT)
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
