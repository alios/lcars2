
module Data.Torrent.Types 
       ( BEncodedT(..)
       , mkBString
       , beDictUTF8
       , beDict
       , beDictLookup
       , parseBencoded
       , beEncodeByteString
       , beString'
       , beInteger
       , beStringUTF8
       ) where


import Data.Map (Map)
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.Generic as UTF8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Attoparsec (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P


data BEncodedT =
  BString !ByteString | BInteger !Integer | 
  BList ![BEncodedT] | BDict ![(BEncodedT, BEncodedT)] 
  deriving (Show, Eq)

mkBString = BString . UTF8.fromString

beEncodeByteString :: BEncodedT -> ByteString
beEncodeByteString (BString bs) = 
  let l = BS.length bs
      pfix= UTF8.fromString $ show l ++ ":"
  in BS.concat [pfix,bs]
beEncodeByteString (BInteger i) =
  UTF8.fromString $ "i" ++ show i ++ "e"
beEncodeByteString (BList l) =
  let ls = BS.concat $ map beEncodeByteString l
  in BS.concat [UTF8.fromString "l", ls, UTF8.fromString "e"]
beEncodeByteString (BDict d) =     
  let r [] = BS.empty
      r ((k,v):rs) = BS.concat $ [beEncodeByteString k, beEncodeByteString v] ++ [(r rs)]
  in BS.concat [UTF8.fromString "d", r d, UTF8.fromString "e"]
     
beString' :: BEncodedT -> ByteString
beString' (BString bs) = bs
beString' v = error $ "beString' called on a: " ++ show v

beStringUTF8 = UTF8.toString . beString'

beInteger :: BEncodedT -> Integer
beInteger (BInteger i) = i
beInteger v = error $ "beInteger called on a: " ++ show v

beList :: BEncodedT -> [BEncodedT]
beList (BList l) = l
beList v = error $ "beList called on a: " ++ show v


beDictLookup :: String -> BEncodedT -> BEncodedT
beDictLookup k = maybe (error $ "unable to lookup dict key " ++ k) id . beDict k

beDict :: String -> BEncodedT -> Maybe BEncodedT
beDict k d = Map.lookup k $ beDictUTF8 d

beDict' :: BEncodedT -> Map ByteString BEncodedT
beDict' (BDict d) = Map.fromList [(beString' k, v) | (k,v) <- d]
beDict' v = error $ "beDict' called on a: " ++ show v

beDictUTF8 :: BEncodedT -> Map String BEncodedT
beDictUTF8 (BDict d) = Map.fromList [(beStringUTF8 k, v) | (k,v) <- d]
beDictUTF8 v = error $ "beDict called on a: " ++ show v


  
parseString :: Parser BEncodedT
parseString = do
  l <- P.decimal
  _ <- P.char ':'
  fmap BString $ P.take l

parseInteger :: Parser BEncodedT
parseInteger = do
  _ <- P.char 'i'
  d <- fmap BInteger $ P.signed P.decimal
  _ <- P.char 'e'
  return d

parseList :: Parser BEncodedT
parseList = do 
  _ <- P.char 'l'
  fmap BList $ P.manyTill parseBencoded $ P.try $ P.char 'e'
  
parseDict :: Parser BEncodedT
parseDict = do  
  _ <- P.char 'd'
  let kvp = do 
        k <- parseString
        v <- parseBencoded
        return (k,v)
  fmap BDict $ P.manyTill kvp $ P.try $ P.char 'e'
    
parseBencoded :: Parser BEncodedT
parseBencoded = P.choice [parseString, parseInteger, parseList, parseDict]
  
                

       