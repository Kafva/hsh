module Sha256 (hash) where

import Control.Monad.Reader
import Data.Foldable (foldl', foldlM)
import Data.Binary (Word8, Word32)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL, rotateR)
import Log (trace', trace'')
import Types (Config)
import Util (padSha1Input,
             word8ArrayToHexArray,
             word8toWord32ArrayBE,
             word32ArrayToBlocks,
             word32ArrayToWord8ArrayBE)
{-
 - https://www.ietf.org/rfc/rfc6234.txt
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    trace' "output: %s" (show [1,2,3]) $ []

