module Md5 (hash) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary

import Data.Bits ((.&.), (.|.), complement, xor)  -- '&', '|' etc.
import Types

{-
    A 16 byte buffer divided into 4 (32 bit) registers is used to compute
    the digest (a b c d).
-}
data Digest = Digest {
    a :: Word32,
    b :: Word32,
    c :: Word32,
    d :: Word32
}

{-
    (1) PADDING BITS
    Append a '1' bit and fill with '0' until the bit-length of the
    input adheres to:
        input % 512 == 448 ~
        input % 64  == 56

    Input will always be a multiple of 8.
    Padding should be performed even if the input length already has 448
    as the remainder mod 512.
-}
padBlock :: [Word8] -> [Word8]
padBlock bytes = if (mod (length bytes) (div 512 8) /= (div 448 8))
                 then padBlock $ bytes ++ [0x0]
                 else bytes


{-
    (2) APPEND LENGTH
    Append the 64 bit representation of the original length of the message
-}
appendLength :: [Word8] -> Word64 -> [Word8]
appendLength bytes len = bytes ++ (ByteStringLazy.unpack $ Binary.encode len)


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

{-
    https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

    Hash algorithms map a variable length bit-string onto a fixed length
    bit-string, 128 bits (16 bytes) in the case of MD5.
-}
hash :: [Char] -> [Word8]
hash inputData = do
    let byteString :: ByteStringLazy.ByteString = Binary.encode inputData
    let bytes :: [Word8] = ByteStringLazy.unpack byteString
    let originalLength :: Word64 = fromIntegral $ length bytes

    -- Append 1 bit to the input
    let padded = padBlock $ bytes ++ [0b1000_0000]

    -- Append 64 bit representation of the original length
    -- The resulting array will be evenly divisible into blocks
    -- of 512 bits (64 bytes)
    let blocks = appendLength padded originalLength

    let digest = Digest {
        a = 0x0123_4567,
        b = 0x89ab_cdef,
        c = 0xfedc_ba98,
        d = 0x7654_3210
    }

    -- Each 512 bit block is split into 16 words (each being 32-bit)
    blocks

