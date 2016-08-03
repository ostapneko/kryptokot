{-# LANGUAGE OverloadedStrings #-}

module XorSpec
 (
  xorRun
) where

import Test.Hspec
import Xor
import Hex

xorRun = do
  describe "xorRepeat" $ do
    it "encrypts a string with repeating key" $ do
      let msg = "Burning 'em, if you ain't quick and nimble"
          key = "ICE"
      xorRepeat key msg `shouldBe` toByteString (HexString "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20")
