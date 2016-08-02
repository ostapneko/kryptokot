module Lib
    ( hexToBase64
    ) where

import Hex
import Base64

hexToBase64 :: HexString -> Base64
hexToBase64 =  fromBS . toByteString
