module Md5 (hash) where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Int as I
import qualified Data.Word as W
import qualified Data.Binary as B
import Data.Bits ((.&.), (.|.), complement, xor)  -- '&', '|' etc.

{-
  A 16 byte buffer divided into 4 (32 bit) registers is used to compute
  the digest (a b c d).
  Word ~ Unsigned
-}
data Digest = Digest {
  a :: W.Word32,
  b :: W.Word32,
  c :: W.Word32,
  d :: W.Word32
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
padBlock :: [B.Word8] -> [B.Word8]
padBlock bytes = if (mod (length bytes) (div 512 8) /= (div 448 8))
                 then padBlock $ bytes ++ [0x0]
                 else bytes


{- (2) APPEND LENGTH
  Append the 64 bit representation of the original length of the message
-}
appendLength :: [B.Word8] -> I.Int64 -> [B.Word8]
appendLength bytes len = bytes ++ (BL.unpack $ B.encode len)


-- Auxillary functions --
-- Each of the auxillary functions are defined to act over bits
-- in each word and map 3 words onto 1.

f :: B.Word8 -> B.Word8 -> B.Word8 -> B.Word8
f x y z = (x .&. y) .|. ((complement x) .&. z)

g :: B.Word8 -> B.Word8 -> B.Word8 -> B.Word8
g x y z = (x .&. z) .|. (y .&. (complement z))

h :: B.Word8 -> B.Word8 -> B.Word8 -> B.Word8
h x y z = xor (xor x y) z

i :: B.Word8 -> B.Word8 -> B.Word8 -> B.Word8
i x y z = xor y $ x .&. (complement z)

{-
  https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

  Hash algorithms map a variable length bit-string onto a fixed length
  bit-string, 128 bits (16 bytes) in the case of MD5.
-}
hash :: BL.ByteString -> [B.Word8]
hash a = do
   let bytes = BL.unpack a
   let original_len = fromIntegral(length bytes) :: I.Int64

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


