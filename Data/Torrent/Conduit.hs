module Data.Torrent.Conduit 
       ( sinkBencoded
       , sinkBdecoded
       , conduitBencoded
       , conduitBdecoded
       ) where

import Data.Torrent.Types
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Attoparsec

sinkBencoded :: (MonadThrow m) => Consumer ByteString m BEncodedT
sinkBencoded = sinkParser parseBencoded

conduitBencoded :: (MonadThrow m) => Conduit ByteString m (PositionRange, BEncodedT)
conduitBencoded = conduitParser parseBencoded

sinkBdecoded :: (MonadThrow m) => Consumer BEncodedT m ByteString
sinkBdecoded = 
  await >>= (maybe sinkBdecoded (return . beEncodeByteString))

conduitBdecoded :: (MonadThrow m) => Conduit BEncodedT m ByteString
conduitBdecoded = awaitForever $ yield . beEncodeByteString 
