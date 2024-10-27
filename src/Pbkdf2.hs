module Pbkdf2 (deriveKey) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Types (Config (algorithm))
import Log (trace')


-- Pbkdf2 usually usess HMAC-SHA1 as its PRF function
-- FIPS 198-1:
-- https://csrc.nist.gov/publications/fips/fips198-1/FIPS-198-1_final.pdf
-- https://datatracker.ietf.org/doc/html/rfc2104

-- F (P, S, c, i) = U_1 \xor U_2 \xor ... \xor U_c



{-
 - https://www.ietf.org/rfc/rfc2898.txt
 -}
deriveKey :: [Word8] -> [Word8] -> Reader Config [Word8]
deriveKey bytes salt = do
    trace' "tmp: %s" "" $
        [0x0]


