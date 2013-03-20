{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Torrent 
       ( module Data.Torrent.Conduit
       , module Data.Torrent.MetaInfo
       ) where

import Data.Torrent.Types
import Data.Conduit
import Crypto.Conduit

import Data.Torrent.Conduit
import Data.Torrent.MetaInfo

instance MetaInfo BEncodedT where
  metaInfo = beDictUTF8
  info m = case (beDict "info" m) of
    Nothing -> error $ "unable to lookup info dict"
    Just i -> beDictUTF8 i
  infoHash m = case (beDict "info" m) of
    Nothing -> fail $ "unable to lookup info dict"
    Just i -> yield i $= conduitBdecoded $$ sinkHash
    


