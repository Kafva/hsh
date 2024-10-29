{- HLINT ignore "Use head" -}
module Util (
    word8ArrayToHexArray,
    word8ArrayToHexString,
    word8toWord32ArrayLE,
    word8toWord32ArrayBE,
    word32ArrayToBlocks,
    word32ArrayToWord8ArrayLE,
    word32ArrayToWord8ArrayBE,
    padMd5Input,
    padSha1Input,
    padEndZero,
    showDigestArray,
    stringToInt,
    intToString
) where

import Types (Block)
import Data.Binary (Word8, Word32, Word64)
import Data.Bits ((.|.), shiftR, shiftL)
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
    let suffix = if length arr <= maxlen
                    then ""
                    else " ... "
    "[" ++ take (length s - 2) s ++ suffix ++ "]"

word8ArrayToHexString :: [Word8] -> Int -> String
word8ArrayToHexString [] _ = ""
word8ArrayToHexString arr maxlen
    | length arr <= maxlen = concatMap (word8ToHexString "") arr
    | otherwise = concatMap (word8ToHexString "") (take maxlen arr) ++ "..."

word32ToWord8ArrayBE :: Word32 -> [Word8]
word32ToWord8ArrayBE word = [fromIntegral (shiftR word 24),
                             fromIntegral (shiftR word 16),
                             fromIntegral (shiftR word 8),
                             fromIntegral word]

word32ToWord8ArrayLE :: Word32 -> [Word8]
word32ToWord8ArrayLE word = [fromIntegral word,
                             fromIntegral (shiftR word 8),
                             fromIntegral (shiftR word 16),
                             fromIntegral (shiftR word 24)]

word64ToWord8ArrayLE :: Word64 -> [Word8]
word64ToWord8ArrayLE word = [fromIntegral word,
                             fromIntegral (shiftR word 8),
                             fromIntegral (shiftR word 16),
                             fromIntegral (shiftR word 24),
                             fromIntegral (shiftR word 32),
                             fromIntegral (shiftR word 40),
                             fromIntegral (shiftR word 48),
                             fromIntegral (shiftR word 56)]

word64ToWord8ArrayBE :: Word64 -> [Word8]
word64ToWord8ArrayBE word = [fromIntegral (shiftR word 56),
                             fromIntegral (shiftR word 48),
                             fromIntegral (shiftR word 40),
                             fromIntegral (shiftR word 32),
                             fromIntegral (shiftR word 24),
                             fromIntegral (shiftR word 16),
                             fromIntegral (shiftR word 8),
                             fromIntegral word]

-- Convert an array of 4 bytes into a Little-endian 32-bit word
--   [0x44 0x33 0x22 0x11] ---> 0x11223344
word8ArrayToWord32LE :: [Word8] -> Word32
word8ArrayToWord32LE bytes =
    if length bytes /= 4
    then 0
    else fromIntegral (bytes!!0) .|.
         shiftL (fromIntegral $ bytes!!1) 8 .|.
         shiftL (fromIntegral $ bytes!!2) 16 .|.
         shiftL (fromIntegral $ bytes!!3) 24

-- Convert an array of 4 bytes into a Big-endian 32-bit word
--   [0x44 0x33 0x22 0x11] ---> 0x44332211
word8ArrayToWord32BE :: [Word8] -> Word32
word8ArrayToWord32BE bytes =
    if length bytes /= 4
    then 0
    else shiftL (fromIntegral $ bytes!!0) 24 .|.
         shiftL (fromIntegral $ bytes!!1) 16 .|.
         shiftL (fromIntegral $ bytes!!2) 8 .|.
         fromIntegral (bytes!!3)

-- Split the given array of bytes into a list of 32 byte entries
-- Returns an empty list if the list is not evenly divisible
word8toWord32ArrayLE :: [Word8] -> [Word32]
word8toWord32ArrayLE [] = []
word8toWord32ArrayLE arr = do
    if mod (length arr) 4 /= 0
    then []
    else word8ArrayToWord32LE (take 4 arr) : word8toWord32ArrayLE (drop 4 arr)

word8toWord32ArrayBE :: [Word8] -> [Word32]
word8toWord32ArrayBE [] = []
word8toWord32ArrayBE arr = do
    if mod (length arr) 4 /= 0
    then []
    else word8ArrayToWord32BE (take 4 arr) : word8toWord32ArrayBE (drop 4 arr)

word32ArrayToBlocks :: [Word32] -> [Block]
word32ArrayToBlocks [] = []
word32ArrayToBlocks arr = do
    if mod (length arr) 16 /= 0
    then []
    else
        take 16 arr
        : word32ArrayToBlocks (drop 16 arr)

word32ArrayToWord8ArrayLE :: [Word32] -> [Word8]
word32ArrayToWord8ArrayLE = concatMap word32ToWord8ArrayLE

word32ArrayToWord8ArrayBE :: [Word32] -> [Word8]
word32ArrayToWord8ArrayBE = concatMap word32ToWord8ArrayBE

showDigestArray :: [Word32] -> Int -> String
showDigestArray digest = word8ArrayToHexArray (word32ArrayToWord8ArrayBE digest)

padSha1Input :: [Word8] -> [Word8]
padSha1Input bytes = do
    let unpaddedBitCount :: Word64 = fromIntegral (8 * length bytes)
    padInput (bytes ++ [0b1000_0000]) ++ word64ToWord8ArrayBE unpaddedBitCount

padMd5Input :: [Word8] -> [Word8]
padMd5Input bytes = do
    let unpaddedBitCount :: Word64 = fromIntegral (8 * length bytes)
    padInput (bytes ++ [0b1000_0000]) ++ word64ToWord8ArrayLE unpaddedBitCount

padEndZero :: [Word8] -> Int -> [Word8]
padEndZero bytes byteCount = if mod (length bytes) byteCount  /= (byteCount-8)
                             then padEndZero (bytes ++ [0x0]) byteCount
                             else bytes

{-
 - SHA1 and MD5 use the same padding method,
 - *except*
 - for the fact that SHA1 appends the message length in Big-endian and MD5 uses
 - Little-endian.
 -
 - The input needs to be padded so that it can be evenly fit into
 - 16 byte blocks. We pad by appending one bit ('1') followed by zeroes
 - until we are 8 bytes (64-bit) short from filling an entire block.
 - The final 8 bytes are filled with the 64-bit representation of the original
 - length (in bits) of the message.
 -
 -}
padInput :: [Word8] -> [Word8]
padInput bytes = if mod (length bytes) 64 /= (64-8)
                 then padInput $ bytes ++ [0x0]
                 else bytes

stringToInt :: String -> Int
stringToInt = read

intToString :: Int -> String
intToString = show

