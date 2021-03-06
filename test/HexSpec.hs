module HexSpec where

import Test.Hspec
import Hex
import qualified Data.ByteString as BS

hexSpecRun = do
  describe "hexPairToWord" $ do
    it "transforms a pair of hex char (uppercase) to a Word8" $ do
      hexPairToWord '0' '0' `shouldBe` 0
      hexPairToWord 'f' 'f' `shouldBe` 255
      hexPairToWord 'a' '2' `shouldBe` 162

  describe "toByteString" $ do
    it "transforms a string encoded in hexa into a ByteString" $ do
      toByteString (HexString "a2ff") `shouldBe` BS.pack [162, 255]
