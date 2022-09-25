{-
  https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

  Hash algorithms map a variable length bit-string onto a fixed length
  bit-string, 128 bits (16 bytes) in the case of MD5.

  The input stream is broken up into 512 bit (64 bytes) blocks
-}
module Md5 (hash) where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Int as I
import qualified Data.Binary as B


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


hash :: BL.ByteString -> [B.Word8]
hash a = do
   let bytes = BL.unpack a
   let original_len = fromIntegral(length bytes) :: I.Int64

   -- Append 1 bit to the input
   let padded = padBlock $ bytes ++ [0b1000_0000]

   -- Append Int64 representation of length
   let withLength = appendLength padded original_len

   withLength


