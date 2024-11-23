module Pbkdf2 (deriveKey) where

import Data.Binary (Word8, Word32)
import Control.Monad.Reader
import Data.Bits (xor)
import Types (Config)
import Log (trace', trace'')
import Util (word32ToWord8ArrayBE, word8ArrayToHexArray)
import Hmac
import Sha1 (hash)

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

-- Accumlate `bytes` split into hLen blocks into one block with xor
mapAccumXor :: [Word8] -> [Word8] -> Reader Config [Word8]
mapAccumXor accumlator bytes
    | length bytes <= hLen = do
        return (zipWith xor accumlator (take hLen bytes))
    | otherwise = do
        mapAccumXor (zipWith xor accumlator (take hLen bytes)) (drop hLen bytes)

-- Return a flat array of [U_1, U_2, ... U_c]
calculateU :: [Word8] -> [Word8] -> [Word8] -> Word32 -> Int -> Reader Config [Word8]
calculateU password bytes accumlator i iterations 
    | i == fromIntegral iterations + 1 = return accumlator
    | otherwise = do
        currentU <- prf password bytes
        nextU <- calculateU password currentU (accumlator ++ currentU) (i+1) iterations
        trace'' "[Pbkdf2] T(i=%d) %s" i (word8ArrayToHexArray nextU 200) nextU

calculateT :: [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
calculateT password salt iterations i = do
    cfg <- ask
    let bytes = salt ++ word32ToWord8ArrayBE (fromIntegral i)
    let start = min i iterations
    let uBlocks = runReader (calculateU password bytes [] (fromIntegral start) iterations) cfg
    mapAccumXor (replicate hLen 0) uBlocks

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
 - Result:
 -     DK = T_1 || T_2 ||  ...  || T_l<0..r-1>
 -
 - We use HMAC-SHA1 as the PRF.
 -
 - https://www.ietf.org/rfc/rfc2898.txt
 -}
deriveKey :: [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
deriveKey password salt iterations derivedKeyLength
    | derivedKeyLength > (2 ^ (32 :: Int) - 1) * hLen = error "Derived key length to large"
    | otherwise = do
    let lastBlockByteCount = mod derivedKeyLength hLen
    -- One extra block if not evenly divisible
    let derivedBlockCount = if lastBlockByteCount == 0 then 
                                div derivedKeyLength hLen else
                                div derivedKeyLength hLen + 1

    -- Calculate each block of the derived key.
    -- mapM acts like fmap for functions that return monads (Reader), all
    -- arguments except `i` are fixed.
    dks <- mapM (calculateT password salt iterations) [1..derivedBlockCount]
    -- Concatenate everything together for the result
    let dk = concat dks

    trace' "[Pbkdf2] output: %s" (word8ArrayToHexArray dk derivedKeyLength) dk
