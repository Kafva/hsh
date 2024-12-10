module Scrypt (Scrypt.deriveKey) where

import Pbkdf2
import Data.Array
import Data.Binary (Word8, Word32)
import Control.Monad.Reader
import Data.Foldable (foldlM)
import Types (Config(..))
import Data.Bits ((.|.), xor, shiftL, shiftR)
import Util (word8toWord32ArrayLE, word32ArrayToWord8ArrayLE, word8ArrayToHexArray)
import Log (trace')

salsaStep :: Array Int Int
salsaStep = listArray (0,4) [7, 9, 13, 18]

salsaRounds :: Array Int (Array Int Int)
salsaRounds = array (0,8) [
        (0, listArray (0,4) [4, 8, 12, 0]),
        (1, listArray (0,4) [9, 13, 1, 5]),
        (2, listArray (0,4) [14, 2, 6, 10]),
        (3, listArray (0,4) [3, 7, 11, 15]),
        (4, listArray (0,4) [1, 2, 3, 0]),
        (5, listArray (0,4) [6, 7, 4, 5]),
        (6, listArray (0,4) [11, 8, 9, 10]),
        (7, listArray (0,4) [12, 13, 14, 15])
    ]

-- R(a,b) (((a) << (b)) | ((a) >> (32 - (b))))
salsaR :: Word32 -> Int -> Word32
salsaR a b = (a `shiftL` b) .|. (a `shiftR` (32 - b))

salsaRunRound :: [Word32] -> (Int, Int, Int) -> Reader Config [Word32]
salsaRunRound bytes32 (_, i, j) = do
    -- Assignment at index: j
    let i0 = (salsaRounds!i)!j
    let a0 = bytes32!!i0
    -- First argument at index: (j+3 % 4)
    let i1 = (salsaRounds!i)!((j+3) `mod` 4)
    let a1 = bytes32!!i1
    -- Second argument at index: (j+2 % 4)
    let i2 = (salsaRounds!i)!((j+2) `mod` 4)
    let a2 = bytes32!!i2
    -- Calculate the new value to assign at bytes32[i0]
    let b0 = a0 `xor` salsaR (a1 + a2) (salsaStep!j)
    return $ take i0 bytes32 ++ [b0] ++ drop (i0+1) bytes32

{-
 - Salsa20/8 Core, hash function that maps a 64 byte input to a 64 byte output.
 - Equivalent to a Word32 array with 16 items.
 - Not collision resistant.
 -}
salsaCore :: [Word8] -> Reader Config [Word8]
salsaCore bytes = do
    when (length bytes /= 64) $ error $
        "Bad input length for Salsa20/8: " ++ show (length bytes) ++ " byte(s)"

    let bytes32 = word8toWord32ArrayLE bytes
    -- Indices for the three loops of the Salsa algorithm:
    --  k:  (for i := 0; i < 8; i += 2) from reference implementation
    --  i:  length of `salsaRounds`
    --  j:  length of each item in `salsaRounds`
    let tpl :: [(Int, Int, Int)] = [(k, i, j) | k <- [0..3],
                                                i <- [0..7],
                                                j <- [0..3]]
    s <- foldlM salsaRunRound bytes32 tpl
    -- Final step, word-wise addition with the original value
    let out = zipWith (+) s bytes32
    trace' "[Salsa] out: %s" (word8ArrayToHexArray (word32ArrayToWord8ArrayLE out) 64) $
        word32ArrayToWord8ArrayLE out


blockMixInner :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
blockMixInner bytes ys i = do
    -- B[i]: current input block
    let b = take 64 (drop (i*64) bytes)
    -- Y[i]: output block from previous calculation
    let y = drop (length ys - 64) ys
    let t = zipWith xor y b
    out <- salsaCore t
    return $ ys ++ out

{-
 -
 -  The accumulatorBytes argument is a concatenation of 128*r byte arrays
 -  from each outer iteration that calls blockMix.
 -
 -  1. X = B[2 * r - 1]
 -
 -  2. for i = 0 to 2 * r - 1 do
 -       T = X xor B[i]
 -       X = Salsa (T)
 -       Y[i] = X
 -
 -  3. (Y[0], Y[2], ..., Y[2 * r - 2],
 -      Y[1], Y[3], ..., Y[2 * r - 1])
 -}
blockMix :: [Word8] -> Int -> Reader Config [Word8]
blockMix accumulatorBytes idx = do
    cfg <- ask
    let r = blockSize cfg
    -- Fetch the block from the previous calculation
    let bytes = drop (128*r*idx) accumulatorBytes
    let blockCount = 2*r

    when (length bytes /= 128*r) $ error $
        "Bad input length for blockMix: " ++ show (length bytes) ++ " byte(s)"

    -- Initialise accumulator with the last block
    let accumulator = drop (64*(blockCount-1)) bytes
    out <- foldlM (blockMixInner bytes) accumulator [0..blockCount-1]

    return $ bytes ++ concat (
             [take (64*i) out     | i <- [0,2..blockCount-2]] ++
             [take (64*(i+1)) out | i <- [0,2..blockCount-1]])


xorBlockMix :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
xorBlockMix vs bytes _ = do
    cfg <- ask
    let r = blockSize cfg
    let j = 1 -- TODO
    let v = take (128*r) (drop (128*r*j) vs)
    blockMix (zipWith xor v bytes) 0


{-
 -  Takes a 128*r byte array as input, indices.
 -  Indices for B[] and V[] are based on 64 byte blocks.
 -  I.e. for r=8 there are 16 slots.
 -
 -  1. X = B
 -
 -  2. for i = 0 to N - 1 do
 -       V[i] = X
 -       X = scryptBlockMix (X)
 -
 -  3. for i = 0 to N - 1 do
 -       j = Integerify (X) mod N
 -              where Integerify (B[0] ... B[2 * r - 1]) is defined
 -              as the result of interpreting B[2 * r - 1] as a
 -              little-endian integer.
 -       T = X xor V[j]
 -       X = scryptBlockMix (T)
 -
 -  4. B' = X
 -}
romMix :: [Word8] -> Reader Config [Word8]
romMix bytes = do
    cfg <- ask
    let r = blockSize cfg
    let n = memoryCost cfg
    when (length bytes /= 128*r) $ error $
        "Bad input length for romMix: " ++ show (length bytes) ++ " byte(s)"

    -- V[]: flat array of (n-1) 128*r blocks
    vs <- foldlM blockMix bytes [0..n-1]

    let x = drop (length vs - 128*r) vs
    foldlM (xorBlockMix vs) x [0..n-1]

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

    -- Run romMix() over each 128*r bytes block of the derived key
    bs <- forM [1..p] $ \i -> romMix (take (128 * r * i) b)
    let b2 = concat bs

    -- New calculation of Pbkdf2 on the password but with b2 as the salt
    Pbkdf2.deriveKey password b2 (derivedKeyLength cfg)

