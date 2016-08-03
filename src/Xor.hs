module Xor
  ( decryptSingle
  ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Bits
import Data.List
import Common

xorSingle :: Word8 -> BS.ByteString -> BS.ByteString
xorSingle key bs =
  let words = BS.unpack bs
  in BS.pack $ map (key `xor`) words

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
