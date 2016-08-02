module Xor
  ( xorSingle
  , findBestKey
  ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Bits
import Data.List
import Common
import qualified Data.Map.Strict as M


xorSingle :: Word8 -> BS.ByteString -> BS.ByteString
xorSingle key bs =
  let words = BS.unpack bs
  in BS.pack $ map (key `xor`) words

frequencyScore :: Word8 -> BS.ByteString -> Float
frequencyScore key bs =
  let decoded = xorSingle key bs
      chars = C8.unpack decoded
  in sum $ map (\ c -> M.findWithDefault 0 c frequencies) chars

findBestKey :: BS.ByteString -> Word8
findBestKey bs =
  let candidates = [minBound..maxBound]
      scores = map (\ c -> (frequencyScore c bs, c)) candidates
      (_, key) = maximum scores
  in key
