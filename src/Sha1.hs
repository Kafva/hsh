module Sha1 (hash) where

import Control.Monad.Reader
import Data.Foldable (foldl')
import Data.Binary (Word8, Word32)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL, rotateR)
import Log (trace', trace'')
import Types (Config, Sha1Digest, Sha1ArrayW, Block)
import Util (padSha1Input,
             word8ArrayToHexArray,
             word8toWord32ArrayBE,
             word32ArrayToBlocks,
             word32ArrayToWord8ArrayBE,
             showSha1Digest)

f :: Int -> Word32 -> Word32 -> Word32 -> Word32
f t b c d
    -- f(t;B,C,D) = (B AND C) OR ((NOT B) AND D)
    | 0 <= t && t <= 19 = (b .&. c) .|. ( (complement b) .&. d)
    -- f(t;B,C,D) = (B AND C) OR (B AND D) OR (C AND D)
    | 40 <= t && t <= 59 = (b .&. c) .|. (b .&. d) .|. (c .&. d)
    -- f(t;B,C,D) = B XOR C XOR D
    | otherwise = foldl' xorReduce b [b, c, d]


getK :: Int -> Word32
getK i
    | i <= 19   = 0x5a827999
    | i <= 39   = 0x6ed9eba1
    | i <= 59   = 0x8f1bbcdc
    | otherwise = 0xca62c1d6


xorReduce :: Word32 -> Word32 -> Word32
xorReduce acc x = xor acc x

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
processW :: Int -> Sha1ArrayW -> Reader Config Sha1Digest -> Reader Config Sha1Digest
processW t w digestH = do
    cfg <- ask
    let digest = (runReader digestH cfg)
    let a = digest!!0
    let b = digest!!1
    let c = digest!!2
    let d = digest!!3
    let e = digest!!4
    let newA = (circularShift a 5) + (f t b c d) + e + (w!!t) + (getK t)
    let newC = circularShift b 30
    let newDigest = [newA, a, newC, c, d]
    trace'' "[t=%d] %s" t (show newDigest) $ newDigest

-- S^n(X)  = (X << n) OR (X >> 32-n)
circularShift :: Word32 -> Int -> Word32
circularShift x n = (rotateL x n) .|. (rotateR x (32 - n))

-- The values from W[16...80] are derived from the initial values in W[0...16].
--
-- W(t) = S^1(W(t-3) XOR W(t-8) XOR W(t-14) XOR W(t-16)) <<< 1
getW :: Int -> Sha1ArrayW -> Sha1ArrayW
getW t w
    | t < 16 || t > 80 = error "Invalid argument: expected index value within [16,80]"
    | otherwise = do
        let ws = [w!!(t-3),
                  w!!(t-8),
                  w!!(t-14),
                  w!!(t-16)]
        let v = circularShift (foldl' xorReduce 0 ws) 1
        (take t w) ++ [v] ++ (drop t w)

processBlock :: Reader Config Sha1Digest -> Block -> Reader Config Sha1Digest
processBlock digestH block = do
    -- Initialise the first 16 slots of W with the values from the current block
    -- and calculate the rest.
    let startW :: Sha1ArrayW = block ++ (replicate (80-16) 0)
    let arrW = foldl' (\w t -> getW t w) startW [16..80]

    foldl' (\d t -> processW t arrW d) digestH [0..80]


{-
 - https://www.ietf.org/rfc/rfc3174.txt
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    let paddedBytes = padSha1Input bytes
    blocks :: [Block] <- trace' "input: %s" (word8ArrayToHexArray paddedBytes 64) $
                         (word32ArrayToBlocks $ word8toWord32ArrayBE paddedBytes)

    -- * Set starting values for H
    let digestH :: Sha1Digest = [0x67452301,
                                 0xefcdab89,
                                 0x98badcfe,
                                 0x10325476,
                                 0xc3d2e1f0]

    -- * Process each block
    finalDigest <- processBlock digestH (blocks!!0)

    trace' "output: %s" (word8ArrayToHexArray (word32ArrayToWord8ArrayBE finalDigest) 20) $
        word32ArrayToWord8ArrayBE finalDigest

