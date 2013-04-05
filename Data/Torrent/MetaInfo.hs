{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Data.Torrent.MetaInfo (PieceID, MetaInfo (..), MetaInfoConduits (..))  where

import Prelude hiding (FilePath)
import Data.Maybe (fromJust)
import Data.Torrent.Types
import Crypto.Hash.SHA1

import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Binary
import Crypto.Conduit
import System.IO hiding (FilePath)
import qualified Data.Serialize as Ser


import Filesystem.Path.CurrentOS

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
  infoFilesL :: t -> Maybe [(String, BEncodedT)]
  infoFilesL m = case (Map.lookup "files" $ info m) of
    Nothing -> Nothing
    Just d -> Just $ beDictUTF8L d
  infoLengthFiles :: t -> Either Integer (Map String BEncodedT)
  infoLengthFiles m = case infoLengthFilesL m of
    Left l -> Left l
    Right fs -> Right $ Map.fromList fs
  infoLengthFilesL :: t -> Either Integer [(String, BEncodedT)]
  infoLengthFilesL m = 
    let l  = infoLength m
        fs = infoFilesL m
        i = info m
        lfs = (l,fs)
    in case lfs of
      (Nothing, Nothing) -> error $ "unable to lookup either 'length' or 'files' in: " ++ show i 
      (Just l', Nothing) -> Left l'
      (Nothing, Just fs') -> Right fs'
      (Just _, Just _) -> error $ "error, both 'length' and 'files' in: " ++ show i
  
type PieceID = Integer

class (MetaInfo t) => MetaInfoConduits t where
  pieceValid :: t -> FilePath -> PieceID -> IO Bool
  pieceValid m fp i = runResourceT $ do
    let (_, src) = pieceConduits m fp i
    src $$ pieceValidator m i 
  pieceValidator :: (MonadResource m) => t -> PieceID -> Consumer ByteString m Bool
  pieceValidator m i =
    let p :: Either String SHA1 
        p = Ser.decode $ (infoPieces m) !! fromInteger i
    in case (p) of
      Left err -> do fail err
      Right d -> do
        d' <- (isolate $ fromInteger $ infoPieceLength m) =$= sinkHash 
        return $ d' == d
  pieceConduits :: (MonadResource m) => 
               t -> FilePath -> PieceID -> (Consumer ByteString m (), Producer m ByteString)
  pieceConduits m fp i =
    let pl = infoPieceLength m
        i'' = pl  * i
        iName = (decodeString $ infoName m)
        ps =  findPieces i'' pl $ case (infoLengthFilesL m) of
          Right fs' -> map fileInfoDecode fs'
          Left l -> [(infoName m, l)]  
        dir = case (infoLengthFilesL m) of
          Right _ -> fp </> iName
          Left _ -> fp
    in (piecesSink dir ps, piecesSource dir ps)
  

                                  
    
piecesSink :: (MonadResource m) => 
              FilePath -> [(FilePath, Integer, Integer)] -> Consumer ByteString m ()
piecesSink _ [] = return ()
piecesSink d ((fn, p, l):fs) = 
  let sink = sinkIOHandle $ openBin WriteMode p l (d </> fn) 
  in do  
    (isolate $ fromInteger l) =$= sink
    piecesSink d fs
    
piecesSource :: (MonadResource m) => 
                FilePath -> [(FilePath, Integer, Integer)] -> Producer m ByteString
piecesSource d ((fn, p, l):fs) = 
  let src = sourceIOHandle $ openBin ReadMode p l (d </> fn) 
  in do
    src =$= (isolate $ fromInteger l)
    piecesSource d fs

openBin :: IOMode -> Integer -> Integer -> FilePath -> IO Handle
openBin mode p l dir = do
  h <- openBinaryFile (encodeString dir) mode
  hSeek h AbsoluteSeek p
  return h

fileInfoDecode :: (String, BEncodedT) -> (String, Integer)
fileInfoDecode (k, v) = (k, beInteger $ v)  
  
findPieces :: Integer -> Integer -> [(String, Integer)] -> [(FilePath, Integer, Integer)]  
findPieces p l ((fn, fl):fs) =
  let pb = p < fl
      pe = (p + l) < fl
  in if (not pb)
     then findPieces (p - fl) l fs
     else (decodeString fn, p, l) : 
          if (pe) then []
          else findPieces 0 (l - fl) fs
       