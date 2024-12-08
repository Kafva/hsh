module Scrypt (Scrypt.deriveKey) where

import Pbkdf2
import Data.Binary (Word8, Word32)
import Control.Monad.Reader
import Data.Foldable (foldlM)
import Types (Config(..))
import Data.Bits ((.|.), xor, shiftL, shiftR)
import Util (word8toWord32ArrayLE, word32ArrayToWord8ArrayLE)
import Log (trace')

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

salsaRunRound :: [Word32] -> (Int, Int, Int) -> Reader Config [Word32]
salsaRunRound bytes32 (_, i, j) = do
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

{-
 - Salsa20/8 Core, hash function that maps a 64 byte input to a 64 byte output.
 - Equivalent to a Word32 array with 16 items.
 - Not collision resistant.
 -}
salsaCore :: [Word8] -> Reader Config [Word8]
salsaCore bytes = do
    let bytes32 = word8toWord32ArrayLE bytes
    -- Indices for the three loops of the Salsa algorithm:
    --  k:  (for i := 0; i < 8; i += 2) from reference implementation
    --  i:  length of `salsaRounds`
    --  j:  length of each item in `salsaRounds`
    let indices :: [(Int, Int, Int)] = [(k, i, j) | k <- [0..3], i <- [0..7], j <- [0..3]]
    s <- foldlM salsaRunRound bytes32 indices
    -- Final step, word-wise addition with the original value
    let out = zipWith (+) s bytes32
    return $ word32ArrayToWord8ArrayLE out


blockMixInner :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
blockMixInner ys bytes i = do
    cfg <- ask
    let r = blockSize cfg

    let y = drop (length ys - r) ys
    let block = take r (drop (i*r) bytes)

    next <- salsaCore (zipWith xor y block)
    return $ ys ++ next

-- The input is raw bytes, it is divided into 64 byte blocks
blockMix :: [Word8] -> Reader Config [Word8]
blockMix bytes = do
    cfg <- ask
    let r = blockSize cfg

    let accumulator = take r (drop (2*r-1) bytes)
    out <- foldlM (blockMixInner accumulator) bytes [0..(2*r-1)]

    -- (Y[0], Y[2], ..., Y[2 * r - 2],
    --  Y[1], Y[3], ..., Y[2 * r - 1])
    return $ [out!!i | i <- [0,2..(2*r-2)]] ++ 
             [out!!(1+i) | i <- [0,2..(2*r-1)]]
    

romMix :: [Word8] -> Reader Config [Word8]
romMix bytes = do
    cfg <- ask
    let r = blockSize cfg
    
    -- WIP
    v <- forM [0..memoryCost cfg - 1] $ \i -> blockMix (take r (drop (r*i) bytes))
    return $ concat v
    -- v <- forM [0..(memoryCost cfg) - 1] $ \i -> do 
    --     let j = block!!(2 * r - 1)

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
    -- Salsa test:
    -- let salsaIn :: [Word32] = [0..15]
    -- salsaCore (word32ArrayToWord8ArrayLE salsaIn)

    let r = blockSize cfg
    let p = parallelisationParam cfg
    let outLen = p * 128 * r

    -- Calculate a Pbkdf2 key for the provided password and salt
    b <- Pbkdf2.deriveKey password salt outLen

    -- Run romMix() over each block (128*r bytes) of the derived key
    b2 <- romMix b

    -- New calculation of Pbkdf2 on the password but with b2 as the salt
    Pbkdf2.deriveKey password b2 (derivedKeyLength cfg)

   
