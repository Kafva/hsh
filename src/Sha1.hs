module Sha1 (hash) where

import Control.Monad.Reader
import Data.Binary (Word8)
import Log (trace', trace'')
import Types (Config)

{-
 - https://www.ietf.org/rfc/rfc3174.txt
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    return bytes
