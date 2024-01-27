module Sha1 (hash) where

import Control.Monad.Reader
import Data.Binary (Word8, Word32)
import Log (trace')
import Types (Config)
import Util (padInput,
             word32ToWord8Array,
             word8ArrayToHexArray,
             word8toWord32Array,
             word32ArrayToBlocks)


{-
 - https://www.ietf.org/rfc/rfc3174.txt
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    let paddedBytes = padInput bytes
    blocks <-  trace' "input: %s" (word8ArrayToHexArray paddedBytes 64) $
               (word32ArrayToBlocks $ word8toWord32Array paddedBytes)

    let startDigest :: [Word32] = [0x67452301,
                                   0xefcdab89,
                                   0x98badcfe,
                                   0x10325476,
                                   0xc3d2e1f0]


    return $ concatMap word32ToWord8Array startDigest
