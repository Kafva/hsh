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

-- Accumlate the hLen blocks of `bytes` into one block with xor
mapAccumXor :: [Word8] -> [Word8] -> Reader Config [Word8]
mapAccumXor accumlator bytes
    | length bytes <= hLen = do
        return (zipWith xor accumlator (take hLen bytes))
    | otherwise = do
        mapAccumXor (zipWith xor accumlator (take hLen bytes)) (drop hLen bytes)

-- Return a flat array of [U_1, U_2, ... U_c]
calculateU :: [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
calculateU password accumlator i iterations 
    | i == iterations + 1 = return accumlator
    | otherwise = do
        -- Pick out the block from the previous iteration from the accumlator
        currentU <- prf password ((reverse . take hLen . reverse) accumlator)
        calculateU password (accumlator ++ currentU) (i+1) iterations

calculateT :: [Word8] -> [Word8] -> Int -> Int -> Reader Config [Word8]
calculateT password salt iterations blockIndex = do
    cfg <- ask
    let bytes = salt ++ word32ToWord8ArrayBE (fromIntegral blockIndex)
    accumlator <- prf password bytes
    let blocksU = runReader (calculateU password accumlator 2 iterations) cfg
    mapAccumXor (replicate hLen 0) blocksU

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
    let derivedBlockCount = if lastBlockByteCount == 0 then 
                                div derivedKeyLength hLen else
                                -- One extra block if not evenly divisible
                                div derivedKeyLength hLen + 1

    -- Calculate each block of the derived key.
    -- mapM acts like fmap for functions that return monads (Reader), all
    -- arguments except `i` are fixed.
    ts <- mapM (calculateT password salt iterations) [1..derivedBlockCount]
    -- Concatenate all blocks together for the result
    dk <- trace' "[Pbkdf2] derivedBlockCount: %d\n" derivedBlockCount $ concat ts

    trace' "[Pbkdf2] output: %s" (word8ArrayToHexArray dk derivedKeyLength) dk
