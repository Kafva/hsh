module Md5 (hash) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary
import qualified Data.Word (Word32)
import qualified Data.Int (Int64)

import Data.Bits ((.&.), (.|.), complement, xor)  -- '&', '|' etc.
import Types

{-
  A 16 byte buffer divided into 4 (32 bit) registers is used to compute
  the digest (a b c d).
  Word ~ Unsigned
-}
data Digest = Digest {
  a :: Word32,
  b :: Word32,
  c :: Word32,
  d :: Word32
}

{-| (1) PADDING BITS
    Append a '1' bit and fill with '0' until the bit-length of the
    input adheres to:
        input % 512 == 448 ~
        input % 64  == 56

  Input will always be a multiple of 8.
  Padding should be performed even if the input length already has 448
  as the remainder mod 512.
-}
padBlock :: [Byte] -> [Byte]
padBlock bytes = if (mod (length bytes) (div 512 8) /= (div 448 8))
                 then padBlock $ bytes ++ [0x0]
                 else bytes


{- (2) APPEND LENGTH
  Append the 64 bit representation of the original length of the message
-}
appendLength :: [Byte] -> Int64 -> [Byte]
appendLength bytes len = bytes ++ (ByteStringLazy.unpack $ Binary.encode len)


-- Auxillary functions --
-- Each of the auxillary functions are defined to act over bits
-- in each word and map 3 words onto 1.
f :: Byte -> Byte -> Byte -> Byte
f x y z = (x .&. y) .|. ((complement x) .&. z)

g :: Byte -> Byte -> Byte -> Byte
g x y z = (x .&. z) .|. (y .&. (complement z))

h :: Byte -> Byte -> Byte -> Byte
h x y z = xor (xor x y) z

i :: Byte -> Byte -> Byte -> Byte
i x y z = xor y $ x .&. (complement z)

{-
  https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

  Hash algorithms map a variable length bit-string onto a fixed length
  bit-string, 128 bits (16 bytes) in the case of MD5.
-}
hash :: ByteStringLazy.ByteString -> [Byte]
hash inputData = do
    let bytes = ByteStringLazy.unpack inputData
    let original_len = fromIntegral(length bytes) :: Int64

    -- Append 1 bit to the input
    let padded = padBlock $ bytes ++ [0b1000_0000]

    -- Append Int64 representation of the original length
    -- The resulting array will be evenly divisible into blocks
    -- of 512 bits (64 bytes)
    let blocks = appendLength padded original_len

    let digest = Digest {
        a = 0x0123_4567,
        b = 0x89ab_cdef,
        c = 0xfedc_ba98,
        d = 0x7654_3210
    }

    -- Each 512 bit block is split into 16 words (each being 32-bit)

    blocks


