module Main where

import Lib
import System.Environment
import Hex
import Xor
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1", "3"] -> run13
    _ -> error "Unknown challenge"

run13 :: IO ()
run13 = do
  let msg = HexString "1B37373331363F78151B7F2B783431333D78397828372D363C78373E783A393B3736"
      bsMsg = toByteString msg
      key = findBestKey  bsMsg
      dec = xorSingle key bsMsg
  C8.putStrLn dec
