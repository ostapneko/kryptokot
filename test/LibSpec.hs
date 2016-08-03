module LibSpec where

import Test.Hspec
import Lib
import Base64
import Hex

libSpecRun = do
  describe "hexToBase64" $ do
    it "convert an hexadecimal string to a base 64 string" $ do
      toString (hexToBase64 (HexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")) `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
