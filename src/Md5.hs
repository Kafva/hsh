module Md5 where
import qualified Data.ByteString.Lazy as BL
import Data.Word as W

import Data.Text.Lazy.Encoding as TLE


{-
  https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

  Hash algorithms map a variable length bit-string onto a fixed length 
  bit-string, 128 bits (16 bytes) in the case of MD5.

  The input stream is broken up into 512 bit (64 bytes) blocks
  Padding is ALWAYS performed as follows:
    1. Append '1' bit
    2. Fill with '0' untill the bit-length of the input mets:
        input % 512 == 448

  Our input will always be a multiple of 8

-}

padBlock :: [W.Word8] -> [W.Word8]
--padBlock :: [Char] -> [Char]
padBlock bytes = if (mod (length bytes) 512 /= 448)
                 then bytes ++ [0x0]
                 else bytes


hash :: BL.ByteString -> [W.Word8]
hash a = do
   padBlock $ BL.unpack a

