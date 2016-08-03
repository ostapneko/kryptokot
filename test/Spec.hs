import Test.Hspec

import HexSpec
import Base64Spec
import LibSpec
import XorSpec

main :: IO ()
main = hspec $ do
  hexSpecRun
  b64Run
  libSpecRun
  xorRun
