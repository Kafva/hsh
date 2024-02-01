{-# LANGUAGE TemplateHaskell #-}

module Sha256 (hash) where

import Control.Monad.Reader
import Data.Foldable (foldl', foldlM)
import Data.Binary (Word8, Word32)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL, rotateR)
import Log (trace', trace'')
import Types (Config, Block, Sha256Digest)
import Numeric (showHex)
import Util (padSha1Input,
             word8ArrayToHexArray,
             word8toWord32ArrayBE,
             word32ArrayToBlocks,
             word32ArrayToWord8ArrayBE,
             showSha256Digest,
             circularShiftL,
             circularShiftR)
import Template (sha256Table, sha256InitialDigest)

-- CH( x, y, z) = (x AND y) XOR ( (NOT x) AND z)
ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = xor  (x .&. y) ((complement x) .&. z)

-- MAJ( x, y, z) = (x AND y) XOR (x AND z) XOR (y AND z)
maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = foldl' (\acc lhs -> xor acc lhs) 0 
                   [(x .&. y), (x .&. z), (y .&. z)] 

-- BSIG0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x)
bsig0 :: Word32 -> Word32
bsig0 x = foldl' (\acc lhs -> xor acc lhs) 0
                 [(circularShiftR x 2), 
                  (circularShiftR x 13), 
                  (circularShiftR x 22)]

-- BSIG1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x)
bsig1 :: Word32 -> Word32
bsig1 x = foldl' (\acc lhs -> xor acc lhs) 0
                 [(circularShiftR x 6), 
                  (circularShiftR x 11), 
                  (circularShiftR x 25)]

-- SSIG0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x)
ssig0 :: Word32 -> Word32
ssig0 x = foldl' (\acc lhs -> xor acc lhs) 0
                 [(circularShiftR x 7), 
                  (circularShiftR x 18), 
                  (rotateR x 3)]

-- SSIG1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x)
ssig1 :: Word32 -> Word32
ssig1 x = foldl' (\acc lhs -> xor acc lhs) 0
                 [(circularShiftR x 17), 
                  (circularShiftR x 19), 
                  (rotateR x 10)]


processBlock :: Sha256Digest -> Block -> Reader Config Sha256Digest
processBlock digest block = return digest

{-
 - https://www.ietf.org/rfc/rfc6234.txt
 -
 - SHA224-256 operates on
 -  * 32-bit words
 -  * 512-bit blocks
 -  * 32 byte digest
 -
 - SHA384-512 operate on
 -  * 64-bit words
 -  * 1024-bit blocks
 -  * 64 byte digest
 -
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    -- * Pad the input (identical approach to SHA1)
    let paddedBytes = padSha1Input bytes
    blocks :: [Block] <- trace' "input: %s" (word8ArrayToHexArray paddedBytes 64) $
                         (word32ArrayToBlocks $ word8toWord32ArrayBE paddedBytes)

    let digest :: Sha256Digest = $(sha256InitialDigest)

    finalDigest <- processBlock digest (blocks!!0)

    trace' "output: %s" (showSha256Digest finalDigest) $
        word32ArrayToWord8ArrayBE finalDigest

