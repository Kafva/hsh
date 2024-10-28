module Hmac (
    calculate,
    verify
) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Types (Config)
import Log (trace')

-- Inner padding bytes
ipad = [0x36 | _ <- [0..64]]
-- Outer padding bytes
opad = [0x5C | _ <- [0..64]]


{-
 - "Message authentication codes" (MAC) are used to check the integrity of a 
 - message based on a secret key. HMAC requires three inputs:
 -  A hash function to use
 -  A secret key for authentication
 -  A message

 - https://csrc.nist.gov/publications/fips/fips198-1/FIPS-198-1_final.pdf
 - https://www.ietf.org/rfc/rfc2104.txt
 -}
calculate :: [Word8] -> [Word8] -> ([Word8] -> Reader Config [Word8]) -> Reader Config [Word8]
calculate bytes key hashFunction = do
    trace' "tmp: %s" "" $
        [0x0]

verify :: [Word8] -> [Word8] -> ([Word8] -> Reader Config [Word8]) -> Reader Config Bool
verify mac key hashFunction = do
    trace' "tmp: %s" "" $
        True

