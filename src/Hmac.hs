module Hmac (calculate) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Data.Bits (xor)
import Types (Config)
import Log (trace')
import Util (padEndZero, word8ArrayToHexArray)

type HashSignature = [Word8] -> Reader Config [Word8]

{-
 - "Message authentication codes" (MAC) are used to check the integrity of a
 - message based on a secret key. HMAC requires three inputs:
 -  * A hash function to use
 -  * A secret key for authentication (shared between sender and recipient)
 -  * A message
 -  The output size depends on the hash function H
 -
 -  digest = H(key XOR opad, H(key XOR ipad, message))
 -
 -
 - https://csrc.nist.gov/publications/fips/fips198-1/FIPS-198-1_final.pdf
 - https://www.ietf.org/rfc/rfc2104.txt
 -}
calculate :: [Word8] -> [Word8] -> HashSignature -> Reader Config [Word8]
calculate bytes key hashFunction = do
    cfg <- ask
    let paddedKey = padEndZero key 64
    let innerKey = map (xor 0x36)  paddedKey
    let outerKey = map (xor 0x5c)  paddedKey
    let innerDigest = runReader (hashFunction $ innerKey ++ bytes) cfg
    let outerDigest = runReader (hashFunction $ outerKey ++ innerDigest) cfg

    trace' "output: %s" (word8ArrayToHexArray outerDigest 20) outerDigest
