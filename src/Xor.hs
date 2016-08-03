module Xor
  ( decryptSingle
  , xorRepeat
  ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Bits
import Data.List
import Common
import Data.Monoid

xorSingle :: Word8 -> BS.ByteString -> BS.ByteString
xorSingle key bs =
  let words = BS.unpack bs
  in BS.pack $ map (key `xor`) words

xorRepeat :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorRepeat key bs =
  let timesRepeated = (BS.length bs `div` BS.length key) + 1
      repeatedKey = mconcat $ replicate timesRepeated key
  in BS.pack $ BS.zipWith xor bs repeatedKey


keyScore :: Word8 -> BS.ByteString -> Float
keyScore key bs =
  let decoded = xorSingle key bs
  in frequencyScore decoded

findBestKey :: BS.ByteString -> Word8
findBestKey bs =
  let candidates = [minBound..maxBound]
      scores = map (\ c -> (keyScore c bs, c)) candidates
      (_, key) = maximum scores
  in key

decryptSingle :: BS.ByteString -> BS.ByteString
decryptSingle bs =
  let key = findBestKey bs
  in xorSingle key bs
