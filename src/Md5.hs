module Md5 where
import qualified Data.ByteString.Lazy as BL
import Debug.Trace



{-
  https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf

  Hash algorithms map a variable length bit-string onto a fixed length 
  bit-string, 128 bits (16 bytes) in the case of MD5.

  The input stream is broken up into 512 bit (64 bytes) blocks
  Padding is ALWAYS performed as follows:
    1. Append '1' bit
    2. Fill with '0' untill the integer value of the input adhears to
        input % 512 == 448

  Our input will

-}

hash :: BL.ByteString -> String
hash a = do
   bytes <- BL.unpack a
   ""

