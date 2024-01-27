module Sha1 (hash) where

import Control.Monad.Reader
import Data.Binary (Word8)
import Log (trace', trace'')
import Types (Config)
import Util (padInput)


{-
 - https://www.ietf.org/rfc/rfc3174.txt
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    let paddedBytes = padInput bytes
    return paddedBytes
