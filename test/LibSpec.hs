module LibSpec where

import Test.Hspec
import Lib
import Base64
import Hex

libSpecRun = do
  describe "hexToBase64" $ do
    it "convert an hexadecimal string to a base 64 string" $ do
      toString (hexToBase64 (HexString "49276D206B696C6C696E6720796F757220627261696E206C696B65206120706F69736F6E6F7573206D757368726F6F6D")) `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
