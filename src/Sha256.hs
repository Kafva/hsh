{-# LANGUAGE TemplateHaskell #-}

module Sha256 (hash) where

import Control.Monad.Reader
import Data.Foldable (foldl', foldlM)
import Data.Binary (Word8, Word32)
import Data.Bits ((.&.), complement, xor, rotateR, shiftR)
import Log (trace', trace'')
import Types (Config, Block, Sha256Digest, Sha256ArrayW)
import Util (padSha1Input,
             word8ArrayToHexArray,
             word8toWord32ArrayBE,
             word32ArrayToBlocks,
             word32ArrayToWord8ArrayBE,
             showDigestArray)
import Template (sha256Table, sha256InitialDigest)

-- CH( x, y, z) = (x AND y) XOR ( (NOT x) AND z)
ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = xor  (x .&. y) ((complement x) .&. z)

-- MAJ( x, y, z) = (x AND y) XOR (x AND z) XOR (y AND z)
maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = foldl' (\acc rhs -> xor acc rhs) 0
                   [(x .&. y), (x .&. z), (y .&. z)]

-- BSIG0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x)
bsig0 :: Word32 -> Word32
bsig0 x = foldl' (\acc rhs -> xor acc rhs) 0
                 [(rotateR x 2),
                  (rotateR x 13),
                  (rotateR x 22)]

-- BSIG1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x)
bsig1 :: Word32 -> Word32
bsig1 x = foldl' (\acc rhs -> xor acc rhs) 0
                 [(rotateR x 6),
                  (rotateR x 11),
                  (rotateR x 25)]

-- SSIG0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x)
ssig0 :: Word32 -> Word32
ssig0 x = foldl' (\acc rhs -> xor acc rhs) 0
                 [(rotateR x 7),
                  (rotateR x 18),
                  (shiftR x 3)]

-- SSIG1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x)
ssig1 :: Word32 -> Word32
ssig1 x = foldl' (\acc rhs -> xor acc rhs) 0
                 [(rotateR x 17),
                  (rotateR x 19),
                  (shiftR x 10)]

{-
 - T1 = h + BSIG1(e) + CH(e,f,g) + Kt + Wt
 - T2 = BSIG0(a) + MAJ(a,b,c)
 - h = g
 - g = f
 - f = e
 - e = d + T1
 - d = c
 - c = b
 - b = a
 - a = T1 + T2
 -
 -}
processW :: Int -> Sha256ArrayW -> Sha256Digest -> Reader Config Sha256Digest
processW t w digest = do
    let a = digest!!0
    let b = digest!!1
    let c = digest!!2
    let d = digest!!3
    let e = digest!!4
    let f = digest!!5
    let g = digest!!6
    let h = digest!!7
    let t1 = foldl' (+) 0 [h,
                           bsig1 e,
                           ch e f g,
                           $(sha256Table)!!t,
                           w!!t]
    let t2 = (bsig0 a) + (maj a b c)
    let newDigest = [t1 + t2,
                     a,
                     b,
                     c,
                     d + t1,
                     e,
                     f,
                     g]
    trace'' "[t=%d] %s" t (show newDigest) $ newDigest


-- Wt = SSIG1(W(t-2)) + W(t-7) + SSIG0(w(t-15)) + W(t-16)
getW :: Int -> Sha256ArrayW -> Sha256ArrayW
getW t w = do
    let v = foldl' (+) 0 [ssig1 (w!!(t-2)),
                          w!!(t-7),
                          ssig0 (w!!(t-15)),
                          w!!(t-16)]
    (take t w) ++ [v] ++ (drop t w)

processBlock :: Sha256Digest -> Block -> Reader Config Sha256Digest
processBlock digest block = do
    -- Initialise the first 16 slots of W with the values from the current block
    -- and calculate the rest.
    let startW :: Sha256ArrayW = block ++ (replicate (64-16) 0)
    let arrW = foldl' (\w t -> getW t w) startW [16..63]

    digestResult <- foldlM (\d t -> processW t arrW d) digest [0..63]

    return $ zipWith (+) digest digestResult

{-
 - https://www.ietf.org/rfc/rfc6234.txt
 -
 - SHA256: 32 byte digest
 - SHA224: 28 byte digest
 - Both operate on
 -  * 32-bit words
 -  * 512-bit blocks
 -
 - SHA224 only differs in the initial values used and the output length.
 -
 - XXX: rotateL and rotateR are equivalent to shiftL and shiftR for unbounded
 - number types like Integer. For Word* types, rotate* acts as a circular
 - rotation, extending the provided value to fill out all bits.
 - The shift functions do not use rotation.
 -
 - (ghci) showBin (rotateR (0x1 :: Word8) 1) ""
 - "10000000"
 - (ghci) showBin (shiftR (0x1 :: Word8) 1) ""
 - "0"
 -
 -}
hash :: [Word8] -> Int -> Reader Config [Word8]
hash bytes digestLength = do
    -- * Pad the input (identical approach to SHA1)
    let paddedBytes = padSha1Input bytes
    blocks :: [Block] <- trace' "input: %s" (word8ArrayToHexArray paddedBytes 64) $
                         (word32ArrayToBlocks $ word8toWord32ArrayBE paddedBytes)

    let digest :: Sha256Digest = if digestLength == 32
                                 then $(sha256InitialDigest)
                                 else [0xc1059ed8,
                                       0x367cd507,
                                       0x3070dd17,
                                       0xf70e5939,
                                       0xffc00b31,
                                       0x68581511,
                                       0x64f98fa7,
                                       0xbefa4fa4]

    processedDigest <- foldlM processBlock digest blocks
    let finalDigest = take (div digestLength 4) processedDigest

    trace' "output: %s" (showDigestArray finalDigest digestLength) $
        word32ArrayToWord8ArrayBE finalDigest
