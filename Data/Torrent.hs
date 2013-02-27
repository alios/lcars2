{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Torrent 
       ( module Data.Torrent.Conduit
       , MetaInfo(..)
       ) where

import Data.Torrent.Types
import Data.Torrent.Conduit

import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Binary 

class MetaInfo t where
  metaInfo :: t -> Map String BEncodedT
  announce :: t -> String
  announce = beStringUTF8 . beLookupKey "name" . metaInfo
  info :: t -> Map String BEncodedT
  info = beDictUTF8 . beLookupKey "info" . metaInfo
  infoName :: t -> String
  infoName = beStringUTF8 . beLookupKey "name" . info
  infoPieceLength :: t -> Integer
  infoPieceLength = beInteger . beLookupKey "piece length" . info 
  infoPieces :: t -> [ByteString]
  infoPieces mi = 
    let f bs 
          | (bs == BS.empty) = []
          | (BS.length bs >= 20) = (BS.take 20 bs) : f (BS.drop 20 bs)
          | otherwise = error $ "found invalid piece of length: " ++ show (BS.length bs)
    in f $ beString' . beLookupKey "pieces" $ info mi
         
  infoLength :: t -> Maybe Integer
  infoLength m = case (beLookupKey' "length" $ info m) of
    Nothing -> Nothing
    Just d -> Just $ beInteger d
  infoFiles :: t -> Maybe (Map String BEncodedT)
  infoFiles m = case (beLookupKey' "files" $ info m) of
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


beLookupKey' k m = Map.lookup k m
beLookupKey k m = 
  case (beLookupKey' k m) of
    Nothing -> error $ "unable to lookup key " ++ k ++ " in: " ++ show m
    Just v -> v

propSize :: MetaInfo t => t -> Bool
propSize t =
  let c =  length (infoPieces t)
      pl = infoPieceLength t
      l' = toInteger c * pl
      l = infoLength t
  in case (l) of
    Nothing -> undefined
    Just l'' -> l'' == l'
        
g = runResourceT $ do
  let src = sourceFile "/home/alios/tmp/t.torrent" $= conduitBencoded
  v <- src $$ await
  v' <- case v of
    Nothing -> fail "unable to find bencoded data"
    Just (_, d) -> return d
  let dst = conduitBdecoded =$ sinkFile "/home/alios/tmp/t1.torrent" 
  yield v' $$ dst
  yield v' $$ sinkBdecoded
