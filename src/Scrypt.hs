module Scrypt (deriveKey) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Types (Config(..))

{-
 -  1. Initialize an array B consisting of p blocks of 128 * r octets each:
 -      B[0] || B[1] || ... || B[p - 1] = PBKDF2-HMAC-SHA256 (P, S, 1, p * 128 * r)
 -
 -  2. for i = 0 to p - 1 do
 -        B[i] = scryptROMix (r, B[i], N)
 -
 -  3. DK = PBKDF2-HMAC-SHA256 (P, B[0] || B[1] || ... || B[p - 1], 1, dkLen)
 -
 - https://www.ietf.org/rfc/rfc7914.txt
 - 
 - SALSA20: http://cr.yp.to/snuffle/spec.pdf
 - SCRYPT: http://www.tarsnap.com/scrypt/scrypt.pdf
 -}
deriveKey :: [Word8] -> [Word8] -> Reader Config [Word8]
deriveKey password salt = do
    return [0x0]
