
module Data.Torrent.Types 
       ( BEncodedT(..)
       , mkBString
       , beDictUTF8
       , beDict
       , beDictLookup
       , parseBencoded
       , ClientHandshake(..)
       , parseClientHandshake
       , randomPeerId
       , PeerMessage(..)
       , parseW8RunLength
       , beEncodeByteString
       , beString'
       , beInteger
       , beStringUTF8
       , putBInt
       , getBInt
       ) where


import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Codec.Binary.UTF8.Generic as UTF8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString as PB
import Crypto.Hash.SHA1
import qualified Data.Serialize as Ser
import Crypto.Random.API

data BEncodedT =
  BString !ByteString | 
  BInteger !Integer | 
  BList ![BEncodedT] | 
  BDict ![(BEncodedT, BEncodedT)] 
  deriving (Show, Eq)

-- | After the client protocol handshake, there comes an alternating stream of length prefixes and 'PeerMessage's
data PeerMessage =
  -- | Messages of length zero are keepalives, and ignored. Keepalives are generally sent once every two minutes, but note that timeouts can be done much more quickly when data is expected.
  KeepAlive |
  Choke |
  Unchoke |
  Interested |
  NotInterested |
  -- | The 'Have' message's payload is a single number, the index which that downloader just completed and checked the hash of.
  Have Int |
  -- | 'Bitfield' is only ever sent as the first message. Its payload is a bitfield with each index that downloader has sent set to one and the rest set to zero. Downloaders which don't have anything yet may skip the 'bitfield' message. The first byte of the bitfield corresponds to indices 0 - 7 from high bit to low bit, respectively. The next one 8-15, etc. Spare bits at the end are set to zero.
  Bitfield ByteString |
  -- | 'Request' messages contain an index, begin, and length. The last two are byte offsets. Length is generally a power of two unless it gets truncated by the end of the file. All current implementations use 2 15 , and close connections which request an amount greater than 2 17.
  Request Int Int Int |
  -- | 'Piece' messages contain an index, begin, and piece. Note that they are correlated with request messages implicitly. It's possible for an unexpected piece to arrive if choke and unchoke messages are sent in quick succession and/or transfer is going very slowly.
  Piece Int Int ByteString |
  -- | 'Cancel' messages have the same payload as request messages. They are generally only sent towards the end of a download, during what's called 'endgame mode'. When a download is almost complete, there's a tendency for the last few pieces to all be downloaded off a single hosed modem line, taking a very long time. To make sure the last few pieces come in quickly, once requests for all pieces a given downloader doesn't have yet are currently pending, it sends requests for everything to everyone it's downloading from. To keep this from becoming horribly inefficient, it sends cancels to everyone else every time a piece arrives.
  Cancel Int Int Int
  deriving (Show, Eq)

instance Binary PeerMessage where
  put = putPeerMessage
  get = getPeerMessage

putPeerMessage' :: PeerMessage -> Put
putPeerMessage' KeepAlive = putLazyByteString BL.empty
putPeerMessage' Choke = putWord8 0x00
putPeerMessage' Unchoke = putWord8 0x01
putPeerMessage' Interested = putWord8 0x02
putPeerMessage' NotInterested = putWord8 0x03
putPeerMessage' (Have i) = do
  putWord8 0x04 
  putBInt i
putPeerMessage' (Bitfield bs) = do
  putWord8 0x05
  putByteString bs
putPeerMessage' (Request i b l) = do
  putWord8 0x06
  putBInt i 
  putBInt b 
  putBInt l
putPeerMessage' (Piece i b bs) = do
  putWord8 0x07
  putBInt i
  putBInt b
  putByteString bs
putPeerMessage' (Cancel i b l) = do 
  putWord8 0x08
  putBInt i 
  putBInt b 
  putBInt l

putPeerMessage :: PeerMessage -> Put
putPeerMessage pm = 
  let bs = runPut $ do 
        putPeerMessage' $ pm
        flush
  in do
    putBInt $ BL.length bs
    putLazyByteString bs
    flush

getPeerMessage :: Get PeerMessage
getPeerMessage = do
  l' <- getBInt
  if (l' == 0) then return KeepAlive
    else do
    t <- getWord8
    let l = l' - 1
    case t of 
      0x00 -> return Choke
      0x01 -> return Unchoke
      0x02 -> return Interested
      0x03 -> return NotInterested
      0x04 -> fmap Have getBInt
      0x05 -> fmap Bitfield $ getByteString l
      0x06 -> do
        i <- getBInt 
        b <- getBInt 
        l <- getBInt
        return $ Request i b l
      0x07 -> do
        i <- getBInt
        b <- getBInt
        bs <- getByteString $ l - 8
        return $ Piece i b bs
      0x08 -> do
        i <- getBInt 
        b <- getBInt 
        l <- getBInt
        return $ Cancel i b l
  
putBInt :: (Integral a) => a -> Put
putBInt = putWord32be . fromInteger . toInteger 
  
getBInt :: (Integral a) => Get a 
getBInt = fmap (fromInteger . toInteger) getWord32be

mkBString :: String -> BEncodedT
mkBString = BString . UTF8.fromString

parseW8RunLength :: Parser String
parseW8RunLength = do
  l <- fmap (fromInteger . toInteger) PB.anyWord8
  fmap UTF8.toString $ PB.take l


data ClientHandshake = ClientHandshake {
  hsInfoHash :: SHA1,
  hsPeerId :: ByteString,
  hsReservedBytes :: ByteString
  } deriving (Eq, Show)

handShakeText :: String
handShakeText = "BitTorrent protocol"

parseTorrentMagic :: PB.Parser ()
parseTorrentMagic = do
  hs <- parseW8RunLength
  if (hs /= handShakeText)
    then fail $ "handshake is not '" ++ handShakeText 
         ++ "'" ++  " but '" ++ hs ++ "'"  
    else return ()


parseClientHandshake :: PB.Parser ClientHandshake
parseClientHandshake = do
  -- torrent magic
  PB.try $ parseTorrentMagic
  
  -- reserved bytes
  reservedBytes <- PB.take 8
  
  -- info hash
  ih' <- fmap Ser.decode $ PB.take 20
  h <- case (ih') of
    Left err -> fail $ "unable to decode SHA1 hash: " ++ err
    Right h -> return h
  
  -- peer id 
  peerid <- PB.take 20
  return $ ClientHandshake h peerid reservedBytes


randomPeerId :: IO ByteString  
randomPeerId = fmap (fst. genRandomBytes 20) getSystemRandomGen


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
  
