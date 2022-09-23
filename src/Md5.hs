{-
  https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

  Hash algorithms map a variable length bit-string onto a fixed length
  bit-string, 128 bits (16 bytes) in the case of MD5.

  The input stream is broken up into 512 bit (64 bytes) blocks
-}
module Md5 (hash) where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Word as W
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
padBlock :: [W.Word8] -> [W.Word8]
padBlock bytes = if (mod (length bytes) (div 512 8) /= (div 448 8))
                 then padBlock $ bytes ++ [0x0]
                 else bytes


{- (2) APPEND LENGTH
  Append the 64 bit representation of the original length of the message
-}
appendLength :: [W.Word8] -> I.Int64 -> [W.Word8]
appendLength bytes length = bytes ++ B.encode length

hash :: BL.ByteString -> [W.Word8]
hash a = do
   -- Append 1 bit to the input
   padBlock $ BL.unpack a ++ [0b1000_0000]


