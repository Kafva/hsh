module Pbkdf2 (deriveKey) where

import Data.Binary (Word8, Word32)
import Control.Monad.Reader
import Data.Bits (xor)
import Types (Config)
import Log (trace')
import Util (word32ToWord8ArrayBE)
import Hmac
import Sha1 (hash)
import Data.Foldable (foldl', foldlM)

-- Output length of the underlying hash function (sha1)
hLen :: Int
hLen = 20

-- From the RFC: "In the case of PBKDF2, the "key" is thus the password and the 
-- "text" is the salt". I.e.
--  password --> hmac 'secret key'
--  salt     --> hmac 'message'
prf :: [Word8] -> [Word8] -> Reader Config [Word8]
prf password salt = Hmac.calculate salt password hash hLen

-- -- TODO: four-octet encoding of blockIndex...
-- let message = salt ++ word32ToWord8ArrayBE blockIndex

funcU :: [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
funcU password salt i l
    | i == 0  = prf password salt
    | otherwise = prf password (runReader (funcU password salt (i-1) l))

{-
 - PBKDF2 (P, S, c, dkLen)
 -
 -     T_1 = F (P, S, c, 1) ,
 -     T_2 = F (P, S, c, 2) ,
 -     ...
 -     T_l = F (P, S, c, l) ,
 -
 - where:
 -
 -  F (P, S, c, i) = U_1 \xor U_2 \xor ... \xor U_c
 -
 -     U_1 = PRF (P, S || INT (i)) ,
 -     U_2 = PRF (P, U_1) ,
 -     ...
 -     U_c = PRF (P, U_{c-1}) .
 -
 - We use HMAC-SHA1 as the PRF
 -
 - https://www.ietf.org/rfc/rfc2898.txt
 -}
deriveKey :: [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
deriveKey password salt iterations derivedKeyLength
    | derivedKeyLength > (2 ^ 32 - 1) * hLen = error "Derived key length to large"
    | otherwise = do
    let derivedBlockCount = ceiling (derivedKeyLength / hLen)
    let lastBlockByteCount = derivedKeyLength - (derivedBlockCount - 1) * hLen

    foldl' prf password salt
    trace' "aa" "aa" $ [0x0]
