{-# LANGUAGE TemplateHaskell #-}

module Md5 (hash) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary

import Data.Bits ((.&.), (.|.), complement, xor, shiftL)
import Data.Binary (Word8)

import Data.Word (Word32)
import Template (md5Table)

{-
    The output digest is comprised of 4 32-bit words (16 bytes)
    Field names: https://wiki.haskell.org/Name_clashes_in_record_fields
-}
type Digest = [Word32] -- 4 slots
type Block = [Word32]  -- 16 slots

padBlock :: [Word8] -> [Word8]
padBlock bytes = do
    let bitsLen = 8 * length bytes
    if mod bitsLen 512 /= 448
    then padBlock $ bytes ++ [0x0]
    else bytes

{-
    Each of the auxillary functions are defined to act over bits
    in each word and map 3 32-bit words onto 1.
-}
auxF :: Word32 -> Word32 -> Word32 -> Word32
auxF x y z = (x .&. y) .|. ((complement x) .&. z)

auxG :: Word32 -> Word32 -> Word32 -> Word32
auxG x y z = (x .&. z) .|. (y .&. (complement z))

auxH :: Word32 -> Word32 -> Word32 -> Word32
auxH x y z = xor (xor x y) z

auxI :: Word32 -> Word32 -> Word32 -> Word32
auxI x y z = xor y $ x .&. (complement z)

-- a = b + ((a + F(b,c,d) + X[k] + T[i]) << s)
auxRound1 :: Digest -> Int -> Int -> Int -> Int -> Block -> Int -> Int -> Int -> Word32
auxRound1 dgst a b c d blk k s t = do
    let da = dgst!!a
    let db = dgst!!b
    let dc = dgst!!c
    let dd = dgst!!d

    shiftL (db + (da + (auxF db dc dd) + (blk!!k) + $(md5Table)!!t)) s


combineToWord32 :: [Word8] -> Word32
combineToWord32 bytes =
    if length bytes /= 4
    then 0
    else (shiftL (fromIntegral $ bytes!!0) 24) .|.
         (shiftL (fromIntegral $ bytes!!1) 16) .|.
         (shiftL (fromIntegral $ bytes!!2) 8) .|.
         fromIntegral (bytes!!3)

-- Split the given array of bytes into a list of 32 byte entries
-- Returns an empty list if the list is not evenly divisable
splitWord32 :: [Word8] -> [Word32]
splitWord32 [] = []
splitWord32 arr = do
    if mod (length arr) 4 /= 0
    then []
    else combineToWord32 (take 4 arr) : splitWord32 (drop 4 arr)


splitBlocks :: [Word32] -> [Block]
splitBlocks [] = []
splitBlocks arr = do
    if mod (length arr) 16 /= 0
    then []
    else
        (take 16 arr)
        : splitBlocks (drop 16 arr)

{-
    https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf
    https://www.ietf.org/rfc/rfc1321.txt

    Hash algorithms map a variable length bit-string onto a fixed length
    bit-string, 128 bits (16 bytes) in the case of MD5.
-}
hash :: [Char] -> [Word8]
hash inputData = do
    let byteString :: ByteStringLazy.ByteString = Binary.encode inputData
    let bytes :: [Word8] = ByteStringLazy.unpack byteString

    -- (1) Add padding bits
    -- Append a '1' bit and fill with '0' until the bit-length of the
    -- input adheres to:
    --     input % 512 == 448
    --
    -- We only allow complete bytes in the input data so the input will always
    -- be a multiple of 8.
    let padded = padBlock $ bytes ++ [0b1000_0000]

    -- (2) Append length
    -- Append the 64 bit representation of the original length (in bits)
    let originalLen :: [Word8] = ByteStringLazy.unpack $ Binary.encode
                                                       $ 8 * length bytes

    let startBytes = padded ++ originalLen

    -- (3) Set starting values
    let digest = [0x0123_4567,
                  0x89ab_cdef,
                  0xfedc_ba98,
                  0x7654_3210]

    -- (4) Process message
    -- The blocks are in multiples of 16 byte words, i.e. the digest can be
    -- evenly fit over it.
    --
    --    /* Process each 16-word block. */
    --    For i = 0 to N/16-1 do
    --
    --      /* Copy block i into X. */
    --      For j = 0 to 15 do
    --          Set X[j] to M[i*16+j].
    --      end
    --
    --

    let roundDigest = digest

    let blocks = splitBlocks $ splitWord32 startBytes

    let x = auxRound1 roundDigest 0 1 2 3 (blocks!!0) 0 7 1

    startBytes

