{-# LANGUAGE TemplateHaskell #-}

module Md5 (hash) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary

import Data.Bits ((.&.), (.|.), complement, xor, shiftL, shiftR)
import Data.Binary (Word8, Word32)

import Template (md5Table)

{-
    The output digest is comprised of 4 32-bit words (16 bytes)
    Field names: https://wiki.haskell.org/Name_clashes_in_record_fields
-}
type Digest = [Word32] -- 4 slots
type Block = [Word32]  -- 16 slots

digestNew :: [Int] -> [Word32] -> Digest
digestNew dgstIndices abcd = [abcd!!(dgstIndices!!0),
                              abcd!!(dgstIndices!!1),
                              abcd!!(dgstIndices!!2),
                              abcd!!(dgstIndices!!3)]

word32ToWord8Array :: Word32 -> [Word8]
word32ToWord8Array word = [fromIntegral word,
                           fromIntegral (shiftR word 8),
                           fromIntegral (shiftR word 16),
                           fromIntegral (shiftR word 24)]

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
auxI x y z = xor y (x .&. (complement z))

-- a = b + ((a + F(b,c,d) + X[k] + T[i]) << s)
auxRound :: Digest -> [Int] -> Block -> [Int] -> (Word32 -> Word32 -> Word32 -> Word32) ->
            Digest
auxRound dgst dgstIndices blk kstIndices auxFunction = do
    let a = dgst!!(dgstIndices!!0)
    let b = dgst!!(dgstIndices!!1)
    let c = dgst!!(dgstIndices!!2)
    let d = dgst!!(dgstIndices!!3)
    let k = kstIndices!!0
    let s = kstIndices!!1
    let t = kstIndices!!2
    let newA = shiftL (b + (a + (auxFunction b c d) + (blk!!k) + $(md5Table)!!t)) s
    digestNew dgstIndices [newA, b, c, d]


word8ArrayToWord32 :: [Word8] -> Word32
word8ArrayToWord32 bytes =
    if length bytes /= 4
    then 0
    else (shiftL (fromIntegral $ bytes!!0) 24) .|.
         (shiftL (fromIntegral $ bytes!!1) 16) .|.
         (shiftL (fromIntegral $ bytes!!2) 8) .|.
         fromIntegral (bytes!!3)

-- Split the given array of bytes into a list of 32 byte entries
-- Returns an empty list if the list is not evenly divisable
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
    let startDigest = [0x0123_4567,
                       0x89ab_cdef,
                       0xfedc_ba98,
                       0x7654_3210]

    -- (4) Process message
    -- The blocks are in multiples of 16 byte words, i.e. the digest can be
    -- evenly fit over it.
    -- Run the round function with each auxiliary function as described in the
    -- RFC, updating one slot in the digest for each `auxRound` call.

    let blocks = word32ArrayToBlocks $ word8toWord32Array startBytes

    let round1ABCD :: [[Int]] = [[0, 1, 2, 3],
                                 [3, 0, 1, 2],
                                 [2, 3, 0, 1],
                                 [1, 2, 3, 0]]

    let round1KSI :: [[Int]] = [[ 0,   7,   1 ],  [ 1,  12,   2 ],  [ 2,  17,   3 ],  [ 3,  22,   4 ],
                                [ 4,   7,   5 ],  [ 5,  12,   6 ],  [ 6,  17,   7 ],  [ 7,  22,   8 ],
                                [ 8,   7,   9 ],  [ 9,  12,  10 ],  [10,  17,  11 ],  [11,  22,  12 ],
                                [12,   7,  13 ],  [13,  12,  14 ],  [14,  17,  15 ],  [15,  22,  16 ]]

    let digest  = auxRound startDigest (round1ABCD!!0) (blocks!!0) (round1KSI!!0)  auxF

    concatMap word32ToWord8Array digest
