module Hex
  ( toByteString
  , hexPairToWord
  , HexString(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Word
import qualified Data.Map as M
import Data.Map ((!))
import Data.Bits

newtype HexString = HexString String deriving (Show, Eq)

charWeights :: M.Map Char Word8
charWeights =
  let chars = ['0'..'9'] ++ ['A'..'F']
      pairs = zip chars [0..16]
  in M.fromList pairs

hexPairToWord :: Char -> Char -> Word8
hexPairToWord c1 c2 =
  let leftBits = shift (charWeights ! c1) 4
      rightBits = charWeights ! c2
  in  leftBits .|. rightBits

toByteString :: HexString -> BS.ByteString
toByteString (HexString hs) =
  let go [] acc = acc
      go (c1 : c2 : t) acc = go t $ acc `BS.snoc` hexPairToWord c1 c2
      go _ _ = error "The hexstring should have an even number of characters"
  in go hs BS.empty
