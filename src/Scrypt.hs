module Scrypt (Scrypt.deriveKey) where

import Pbkdf2
import Data.Binary (Word8, Word32)
import Control.Monad.Reader
import Data.Foldable (foldlM)
import Types (Config(..))
import Data.Bits ((.|.), xor, shiftL, shiftR)
import Util (word8toWord32ArrayLE, word32ArrayToWord8ArrayLE)

salsaRounds :: [[Int]]
salsaRounds = [
        [4, 8, 12, 0],
        [9, 13, 1, 5],
        [14, 2, 6, 10],
        [3, 7, 11, 15],
        [1, 2, 3, 0],
        [6, 7, 4, 5],
        [11, 8, 9, 10],
        [12, 13, 14, 15]
    ]

-- R(a,b) (((a) << (b)) | ((a) >> (32 - (b))))
salsaR :: Word32 -> Int -> Word32
salsaR a b = (a `shiftL` b) .|. (a `shiftR` (32 - b))

salsaStep :: Int -> Int
salsaStep j = case j of
            0 -> 7
            1 -> 9
            2 -> 13
            3 -> 18
            _ -> error "Invalid iteration count"

salsaRunRound :: [Word32] -> Int -> Reader Config [Word32]
salsaRunRound bytes32 ij = do
    -- i and j give us the indices inside bytes32 to use from salsaRounds[]
    let i = ij `mod` 8
    let j = ij `mod` 4
    -- Assignment at index: j
    let i0 = (salsaRounds!!i)!!j
    let a0 = bytes32!!i0
    -- First argument at index: (j+3 % 4)
    let i1 = (salsaRounds!!i)!!((j+3) `mod` 4)
    let a1 = bytes32!!i1
    -- Second argument at index: (j+2 % 4)
    let i2 = (salsaRounds!!i)!!((j+2) `mod` 4)
    let a2 = bytes32!!i2
    -- Calculate the new value to assign at bytes32[i0]
    let b0 = a0 `xor` salsaR (a1 + a2) (salsaStep j)
    return $ take i0 bytes32 ++ [b0] ++ drop (i0+1) bytes32


-- Salsa20/8 Core, hash function that maps a 64 byte input to a 64 byte output.
-- Equivalent to a Word32 array with 16 items.
-- Not collision resistant.
salsaCore :: [Word8] -> Reader Config [Word8]
salsaCore bytes = do
    let bytes32 = word8toWord32ArrayLE bytes
    s <- foldlM salsaRunRound bytes32 [0..(4*8)]
    return $ word32ArrayToWord8ArrayLE s


{-
 -
 -  Scrypt(P, S, N, r, p, dkLen):
 -
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
    cfg <- ask
    let r = blockSize cfg
    let p = parallelisationParam cfg
    let outLen = p * 128 * r
    b <- Pbkdf2.deriveKey password salt outLen
    salsaCore b
