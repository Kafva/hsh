module Md5 (hash) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary

import Data.Bits ((.&.), (.|.), complement, xor, shiftL)
import Types

{-
    A 16 byte buffer divided into 4 (32 bit) registers is used to compute
    the digest (a b c d).
-}
data Block = Block {
    a :: Word32,
    b :: Word32,
    c :: Word32,
    d :: Word32
}

padBlock :: [Word8] -> [Word8]
padBlock bytes = do
    let bitsLen = 8 * length bytes
    if mod bitsLen 512 /= 448
    then padBlock $ bytes ++ [0x0]
    else bytes

{-
    Each of the auxillary functions are defined to act over bits
    in each word and map 3 words onto 1.
-}
f :: Word8 -> Word8 -> Word8 -> Word8
f x y z = (x .&. y) .|. ((complement x) .&. z)

g :: Word8 -> Word8 -> Word8 -> Word8
g x y z = (x .&. z) .|. (y .&. (complement z))

h :: Word8 -> Word8 -> Word8 -> Word8
h x y z = xor (xor x y) z

i :: Word8 -> Word8 -> Word8 -> Word8
i x y z = xor y $ x .&. (complement z)


combineToWord32 :: [Word8] -> Word32
combineToWord32 bytes =
    if length bytes /= 4
    then 0
    else shiftL $ fromIntegral (bytes!!0) 24 .|.
         shiftL $ fromIntegral (bytes!!1) 16 .|.
         shiftL $ fromIntegral (bytes!!2) 8 .|.
         fromIntegral bytes!!3

-- Split the given array of bytes into a list of 32 byte entries
-- Returns an empty list if the list is not evenly divisable
splitWord32 :: [Word8] -> [Word32]
splitWord32 [] = []
splitWord32 arr = do
    if mod (length arr) 4 /= 0
    then []
    else combineToWord32 (take 4 arr) : splitWord32 (drop 4 arr)


splitBlocks :: [Word8] -> [Block]
splitBlocks [] = []
splitBlocks arr = [Block{}]

{-
    https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

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

    let blocks = padded ++ originalLen

    -- (3) Set starting values
    let digest = Block {
        a = 0x0123_4567,
        b = 0x89ab_cdef,
        c = 0xfedc_ba98,
        d = 0x7654_3210
    }

    -- (4) Process message
    -- The blocks are in a multiple of 16 byte words, i.e. the digest can be
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

    -- (splitBlocks blocks)


    blocks

