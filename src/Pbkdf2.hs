module Pbkdf2 (deriveKey) where

import Data.Binary (Word8, Word32)
import Control.Monad.Reader
import Data.Bits (xor)
import Types (Config)
import Log (trace')
import Util (word32ToWord8ArrayBE, word8ArrayToHexArray)
import Hmac
import Sha1 (hash)
import Data.Foldable (foldl', foldlM)

-- Output length of the underlying hash function (sha1)
hLen :: Int
hLen = 20

-- From the RFC: "In the case of PBKDF2, the "key" is thus the password and the 
-- "text" is the salt". I.e.
--  password     --> hmac 'secret key'
--  bytes        --> hmac 'message'
--  With bytes being (salt || INT) in the basecase
prf :: [Word8] -> [Word8] -> Reader Config [Word8]
prf password bytes = do
    Hmac.calculate bytes password hash hLen



{-
 - PBKDF2 (P, S, c, dkLen)
 -
 - Outer loop:
 -     T_1 = F (P, S, c, 1) ,
 -     T_2 = F (P, S, c, 2) ,
 -     ...
 -     T_l = F (P, S, c, l) ,
 -
 - Inner loop:
 -  F (P, S, c, i) = U_1 \xor U_2 \xor ... \xor U_c
 -
 -     U_1 = PRF (P, S || INT (i)) ,
 -     U_2 = PRF (P, U_1) ,
 -     ...
 -     U_c = PRF (P, U_{c-1}) .
 -
 -
 - Basically, there is an outer loop for each block of the derivedKey
 - and an inner loop that runs `iterations` times.
 -
 -
 - We use HMAC-SHA1 as the PRF
 -
 - https://www.ietf.org/rfc/rfc2898.txt
 -}


-- Accumlate `bytes` split into hLen blocks into one block with xor
mapAccumXor :: [Word8] -> [Word8] -> Reader Config [Word8]
mapAccumXor acc bytes
    | length bytes <= hLen = do
        return (zipWith xor acc (take hLen bytes))
    | otherwise = do
        mapAccumXor (zipWith xor acc (take hLen bytes)) (drop hLen bytes)
    

-- Return a flat array of [U_1, U_2, ... U_c]
u :: [Word8] -> [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
u password bytes accumlator i limit
    | i == limit = do 
        u_limit <- prf password bytes
        let ret = accumlator ++ u_limit
        return ret
    | otherwise = do 
        u_next <- u password bytes accumlator (i+1) limit
        let ret = accumlator ++ u_next
        return ret

deriveKey :: [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
deriveKey password salt iterations derivedKeyLength
    | derivedKeyLength > (2 ^ (22 :: Int) - 1) * hLen = error "Derived key length to large"
    | otherwise = do
    cfg <- ask
    let lastBlockByteCount = mod derivedKeyLength hLen
    let derivedBlockCount = if lastBlockByteCount == 0 then 
                                div derivedKeyLength hLen else
                                div derivedKeyLength hLen + 1

    let acc :: [Word8] = []
    let s1 = salt ++ word32ToWord8ArrayBE 0x1
    let uBlocks = runReader (u password s1 acc 1 iterations) cfg
    let t1 = runReader (mapAccumXor (replicate hLen 0) uBlocks) cfg

    trace' "[Pbkdf2] output: %s" (word8ArrayToHexArray t1 derivedKeyLength) t1
