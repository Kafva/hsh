module Sha1 (hash) where

import Control.Monad.Reader
import Data.Binary (Word8, Word32)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL, rotateR)
import Log (trace')
import Types (Config)
import Util (padInput,
             word32ToWord8Array,
             word8ArrayToHexArray,
             word8toWord32Array,
             word32ArrayToBlocks)

f :: Int -> Word32 -> Word32 -> Word32 -> Word32
f t b c d
    -- f(t;B,C,D) = (B AND C) OR ((NOT B) AND D)
    | 0 <= t && t <= 19 = (b .&. c) .|. ( (complement b) .&. d)
    -- f(t;B,C,D) = (B AND C) OR (B AND D) OR (C AND D)
    | 40 <= t && t <= 59 = (b .&. c) .|. (b .&. d) .|. (c .&. d)
    -- f(t;B,C,D) = B XOR C XOR D
    | otherwise = xor b (xor c d)


getK :: Int -> Word32
getK i
    | i <= 19   = 0x5a827999
    | i <= 39   = 0x6ed9eba1
    | i <= 59   = 0x8f1bbcdc
    | otherwise = 0xca62c1d6

-- S^n(X)  = (X << n) OR (X >> 32-n)
auxS :: Int -> Word32 -> Word32
auxS n x = (rotateL x n) .|. (rotateR x (32 - n))

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

    -- W(t) = S^1(W(t-3) XOR W(t-8) XOR W(t-14) XOR W(t-16)) <<< 1


    return $ concatMap word32ToWord8Array startDigest
