
module Main (main) where

import Filesystem.Path.CurrentOS
import System.Exit
import Data.Conduit

import Data.Torrent

testTrackers :: [String]
testTrackers = [ "http://localhost:3000/tracker" ]


handleResult t = case t of
  Nothing -> exitFailure
  Just t' -> do 
    tcoded <- yield t' $$ sinkBdecoded 
    print tcoded
    exitSuccess

t1 = "/etc/passwd"
t2 = "/home/alios/tmp/xy/"

main :: IO a
main = do
  let fp = decodeString t2
  t <- mkTorrent fp testTrackers defaultChunkSize 
  handleResult t