
module Main (main) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS
import System.Exit
import System.Environment
import System.Console.GetOpt
import Data.Conduit
import Data.Conduit.Binary
import Data.Torrent

testTrackers :: [String]
testTrackers = [ "http://localhost:3000/tracker" ]


handleResult t = case t of
  Nothing -> exitFailure
  Just t' -> do 
    let n = infoName t'
    runResourceT $ yield t' $= conduitBdecoded $$ sinkFile $ n ++ ".torrent"
    ih <- infoHash t' 
    print $ "Successfully created torrent with infoHash: " ++ show ih
    
    exitSuccess

data Flag =
  Version | Input FilePath | ChunkSize Integer | Tracker String
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['V', '?'] ["version"] (NoArg Version) "show version number"
  , Option ['i'] ["input"] (ReqArg inp "DIR") "input file or directory"
  , Option ['s'] ["chunksize"] (ReqArg cs "INTEGER") "chunk size as base 2 exponent"
  , Option ['t'] ["tracker"] (ReqArg tr "URL") "torrent main tracker"
  ]

inp :: String -> Flag
inp = Input . decodeString 

cs :: String -> Flag
cs i = ChunkSize $ 2 ^ (read i)

tr :: String -> Flag
tr s = Tracker s

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (Prelude.concat errs ++ usageInfo header options))
      
header = "Usage: MkTorrent [OPTION]"

lookupInput :: [Flag] -> FilePath 
lookupInput [] = error "unable to lookup input file in args"
lookupInput ((Input fp):_) = fp
lookupInput (_:fs) = lookupInput fs

lookupTracker :: [Flag] -> String
lookupTracker [] = error "unable to lookup tracker in args"
lookupTracker ((Tracker t):_) = t
lookupTracker (_:fs) = lookupTracker fs

lookupChunkSize :: [Flag] -> Integer
lookupChunkSize [] = defaultChunkSize
lookupChunkSize ((ChunkSize s):_) = s
lookupChunkSize (_:fs) = lookupChunkSize fs

main :: IO a
main = do
  (flags, args) <- getArgs >>= compilerOpts 
  if (Version `elem` flags)
    then do putStr $ usageInfo header options
            exitSuccess
    else do
    let fp = lookupInput flags
        cs = lookupChunkSize flags
        t = lookupTracker flags
    t <- mkTorrent fp [t] cs
    handleResult t
    
  