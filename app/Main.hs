module Main where

import Lib
import System.Environment
import Hex
import Xor
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import Paths_kryptokot
import Set1Challenge4

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1", "3"] -> run13
    ["1", "4"] -> run14
    _ -> error "Unknown challenge"

run13 :: IO ()
run13 = do
  let msg = HexString "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      bsMsg = toByteString msg
      dec = decryptSingle bsMsg
  C8.putStrLn dec

run14 :: IO ()
run14 = do
  path <- getDataFileName "set1challenge4.txt"
  content <- readFile path
  let hexLs = map HexString $ lines content
      ls = map toByteString hexLs
  C8.putStr $ Set1Challenge4.run ls
