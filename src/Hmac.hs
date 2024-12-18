module Hmac (calculate) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Data.Bits (xor)
import Types (Config(..))
import Log (trace')
import Util (padEndZero, word8ArrayToHexArray)

{-
 - "Message authentication codes" (MAC) are used to check the integrity of a
 - message based on a secret key. HMAC requires three inputs:
 -  * A message
 -  * A secret key for authentication (shared between sender and recipient)
 -  * A hash function to use
 -  The output size depends on the hash function H
 -
 -  digest = H(key XOR opad, H(key XOR ipad, message))
 -
 - https://csrc.nist.gov/publications/fips/fips198-1/FIPS-198-1_final.pdf
 - https://www.ietf.org/rfc/rfc2104.txt
 -}
calculate :: [Word8] -> [Word8] -> Reader Config [Word8]
calculate bytes key = do
    cfg <- ask
    let hashFunction = innerAlgorithm cfg

    let paddedKey = padEndZero key 64
    let innerKey = map (xor 0x36) paddedKey
    let outerKey = map (xor 0x5c) paddedKey
    let innerDigest = runReader (hashFunction $ innerKey ++ bytes) cfg
    let outerDigest = runReader (hashFunction $ outerKey ++ innerDigest) cfg

    let hashLen = innerAlgorithmLength cfg
    trace' "[Hmac] output: %s" (word8ArrayToHexArray outerDigest hashLen) outerDigest
