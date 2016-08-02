{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Base64
  ( Base64(..)
  , from1Byte
  , from2Bytes
  , from3Bytes
  , fromBS
  , toString
  ) where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Data.Monoid
import Data.String
import qualified Data.Map as M
import Data.Map ((!))

newtype Base64 = Base64 BS.ByteString deriving (Show, Eq, Monoid, IsString)

clearFirsts :: Int -> Word8 -> Word8
clearFirsts n = flip shiftR n . flip shiftL n

from1Byte :: Word8 -> Base64
from1Byte w =
  let rs = bytesToBase64Words w 0 0
  in Base64 . BS.pack $ take 2 rs

from2Bytes :: Word8 -> Word8 -> Base64
from2Bytes w1 w2 =
  let rs = bytesToBase64Words w1 w2 0
  in Base64 . BS.pack $ take 3 rs


from3Bytes :: Word8 -> Word8 -> Word8 -> Base64
from3Bytes w1 w2 w3 =
  Base64 . BS.pack $ bytesToBase64Words w1 w2 w3

bytesToBase64Words :: Word8 -> Word8 -> Word8 -> [Word8]
bytesToBase64Words w1 w2 w3 =
  let r1 = shiftR w1 2
      r2 = clearFirsts 2 (shiftL w1 4) .|. shiftR w2 4
      r3 = clearFirsts 2 (shiftL w2 2) .|. shiftR w3 6
      r4 = clearFirsts 2 w3
  in [r1, r2, r3, r4]


fromBS :: BS.ByteString -> Base64
fromBS bs =
  let go ws acc =
         case take 3 ws of
           [w1, w2, w3] -> go (drop 3 ws) (acc <> from3Bytes w1 w2 w3)
           [w1, w2] -> acc <> from2Bytes w1 w2 <> Base64 (BS.pack [64])
           [w] -> acc <> from1Byte w <> Base64 (BS.pack [64])
           [] -> acc
  in go (BS.unpack bs) mempty

base64CharTable :: M.Map Word8 Char
base64CharTable =
  let chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/="
  in  M.fromList $ zip [0..] chars

toString :: Base64 -> String
toString (Base64 bs) =
  map (base64CharTable !) $ BS.unpack bs
