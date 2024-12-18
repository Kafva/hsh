{- HLINT ignore "Use head" -}
module Sha1 (hash) where

import Control.Monad.Reader
import Data.Foldable (foldlM)
import Data.Binary (Word32)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL)
import Log (trace')
import Types (Config, Sha1Digest, Sha1ArrayW, Block, HashSignature)
import Util (padSha1Input,
             word8ArrayToHexArray,
             word8toWord32ArrayBE,
             word32ArrayToBlocks,
             word32ArrayToWord8ArrayBE)

f :: Int -> Word32 -> Word32 -> Word32 -> Word32
f t b c d
    -- f(t;B,C,D) = (B AND C) OR ((NOT B) AND D)
    | 0 <= t && t <= 19 = (b .&. c) .|. (complement b .&. d)
    -- f(t;B,C,D) = (B AND C) OR (B AND D) OR (C AND D)
    | 40 <= t && t <= 59 = (b .&. c) .|. (b .&. d) .|. (c .&. d)
    -- f(t;B,C,D) = B XOR C XOR D
    | otherwise = foldl' xor 0 [b, c, d]

getK :: Int -> Word32
getK i
    | i <= 19   = 0x5a827999
    | i <= 39   = 0x6ed9eba1
    | i <= 59   = 0x8f1bbcdc
    | otherwise = 0xca62c1d6

-- The values from W[16...80] are derived from the initial values in W[0...16].
--
-- W(t) = S^1(W(t-3) XOR W(t-8) XOR W(t-14) XOR W(t-16)) <<< 1
getW :: Sha1ArrayW -> Int -> Sha1ArrayW
getW w t
    | t < 16 || t > 80 = error "Invalid argument: expected index value within [16,80]"
    | otherwise = do
        let ws = [w!!(t-3),
                  w!!(t-8),
                  w!!(t-14),
                  w!!(t-16)]
        let v = rotateL (foldl' xor 0 ws) 1
        take t w ++ [v] ++ drop t w

{-
 -
 -  TEMP = S^5(A) + f(t;B,C,D) + E + W(t) + K(t);
 -
 -  E = D;
 -  D = C;
 -  C = S^30(B);
 -  B = A;
 -  A = TEMP;
 -
 -}
processW :: Sha1ArrayW -> Sha1Digest -> Int -> Reader Config Sha1Digest
processW w digest t = do
    let a = digest!!0
    let b = digest!!1
    let c = digest!!2
    let d = digest!!3
    let e = digest!!4
    let newA = rotateL a 5 + f t b c d + e + (w!!t) + getK t
    let newC = rotateL b 30
    let newDigest = [newA, a, newC, c, d]
    -- trace'' "[Sha1] t=%d %s" t (show newDigest) newDigest
    return newDigest

processBlock :: Sha1Digest -> Block -> Reader Config Sha1Digest
processBlock digest block = do
    -- Initialise the first 16 slots of W with the values from the current block
    -- and calculate the rest.
    let startW :: Sha1ArrayW = block ++ replicate (80-16) 0
    let arrW = foldl' getW startW [16..79]

    -- Since `processW` returns a Reader (a Monad) we need to use a fold method
    -- that wraps operations in a monad, `foldM`.
    -- The digest and [0..79] arguments are implicitly provided to each
    -- `processW` invocation.
    digestResult <- foldlM (processW arrW) digest [0..79]

    return $ zipWith (+) digest digestResult


{-
 - https://www.ietf.org/rfc/rfc3174.txt
 -}
hash :: HashSignature
hash bytes = do
    -- * Pad the input
    let paddedBytes = padSha1Input bytes
    blocks :: [Block] <- trace' "[Sha1] input: %s" (word8ArrayToHexArray paddedBytes 64)
                         (word32ArrayToBlocks $ word8toWord32ArrayBE paddedBytes)

    -- * Set starting values
    let digest :: Sha1Digest = [0x67452301,
                                0xefcdab89,
                                0x98badcfe,
                                0x10325476,
                                0xc3d2e1f0]

    -- * Process each block
    finalDigest <- foldlM processBlock digest blocks

    return $ word32ArrayToWord8ArrayBE finalDigest
