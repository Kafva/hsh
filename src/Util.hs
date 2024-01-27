module Util (
    word8ArrayToHexArray,
    word8ArrayToHexString,
    word8toWord32Array,
    word32ToWord8Array,
    word32ArrayToBlocks,
    word64ToWord8Array,
    padInput
) where

import Types (Block)
import Data.Binary (Word8, Word32, Word64)
import Data.Bits ((.|.), rotateR, rotateL)
import Numeric (showHex)
import Data.Char(toLower)

word8ToHexString :: String -> Word8 -> String
word8ToHexString prefix w = do
    let hexValue = map toLower (showHex w "")
    if length hexValue == 1
    then prefix ++ "0" ++ hexValue
    else prefix ++ hexValue

word8ArrayToHexArray :: [Word8] -> Int -> String
word8ArrayToHexArray [] _ = "[]"
word8ArrayToHexArray arr maxlen = do
    let s = concatMap ((++ ", ") . word8ToHexString "0x")
                      (take maxlen arr)
    let suffix = if (length arr) <= maxlen
                    then ""
                    else " ... "
    "[" ++ take (length s - 2) s ++ suffix ++ "]"

word8ArrayToHexString :: [Word8] -> Int -> String
word8ArrayToHexString [] _ = ""
word8ArrayToHexString arr maxlen
    | length arr <= maxlen = concatMap (word8ToHexString "") arr
    | otherwise = concatMap (word8ToHexString "") (take maxlen arr) ++ "..."


word32ToWord8Array :: Word32 -> [Word8]
word32ToWord8Array word = [fromIntegral word,
                           fromIntegral (rotateR word 8),
                           fromIntegral (rotateR word 16),
                           fromIntegral (rotateR word 24)]

word64ToWord8Array :: Word64 -> [Word8]
word64ToWord8Array word = [fromIntegral word,
                           fromIntegral (rotateR word 8),
                           fromIntegral (rotateR word 16),
                           fromIntegral (rotateR word 24),
                           fromIntegral (rotateR word 32),
                           fromIntegral (rotateR word 40),
                           fromIntegral (rotateR word 48),
                           fromIntegral (rotateR word 56)]


-- Convert an array of 4 bytes into a Little-endian 16 byte word
--   [0x44 0x33 0x22 0x11] ---> 0x11223344
word8ArrayToWord32 :: [Word8] -> Word32
word8ArrayToWord32 bytes =
    if length bytes /= 4
    then 0
    else fromIntegral (bytes!!0) .|.
         (rotateL (fromIntegral $ bytes!!1) 8) .|.
         (rotateL (fromIntegral $ bytes!!2) 16) .|.
         (rotateL (fromIntegral $ bytes!!3) 24)


-- Split the given array of bytes into a list of 32 byte entries
-- Returns an empty list if the list is not evenly divisible
word8toWord32Array :: [Word8] -> [Word32]
word8toWord32Array [] = []
word8toWord32Array arr = do
    if mod (length arr) 4 /= 0
    then []
    else word8ArrayToWord32 (take 4 arr) : word8toWord32Array (drop 4 arr)


word32ArrayToBlocks :: [Word32] -> [Block]
word32ArrayToBlocks [] = []
word32ArrayToBlocks arr = do
    if mod (length arr) 16 /= 0
    then []
    else
        (take 16 arr)
        : word32ArrayToBlocks (drop 16 arr)

{-
 - SHA1 and MD5 use the same padding method.
 -
 - The input needs to be padded so that it can be evenly fit into
 - 16 byte blocks. We pad by appending one bit ('1') followed by zeroes
 - until we are 8 bytes (64-bit) short from filling an entire block.
 - The final 8 bytes are filled with the 64-bit representation of the original
 - length (in bits) of the message.
 -
 -}
padInput :: [Word8] -> [Word8]
padInput bytes = do
    let unpaddedBitCount :: Word64 = fromIntegral (8 * length bytes)
    _padInput (bytes ++ [0b1000_0000]) ++
              (word64ToWord8Array unpaddedBitCount)

_padInput :: [Word8] -> [Word8]
_padInput bytes = do
    if (mod (length bytes) 64) /= (64-8)
    then _padInput $ bytes ++ [0x0]
    else bytes

