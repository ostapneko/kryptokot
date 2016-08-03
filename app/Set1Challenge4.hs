module Set1Challenge4
  ( run
  ) where

import qualified Data.ByteString as BS
import Xor
import Common

run :: [BS.ByteString] -> BS.ByteString
run msgs =
  let candidates = map decryptSingle msgs
      scored = map (\ c -> (frequencyScore c, c)) candidates
      (_, res) = maximum scored
  in res
