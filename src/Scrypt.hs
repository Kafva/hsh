module Scrypt (Scrypt.deriveKey) where

import Pbkdf2
import Data.Array
import Data.Binary (Word8, Word32, Word64)
import Control.Monad.Reader
import Data.Foldable (foldlM)
import Types (Config(..))
import Data.Bits ((.|.), xor, shiftL, shiftR)
import Util (word8toWord32ArrayLE, word32ArrayToWord8ArrayLE)

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
 - bytes:  64 byte array
 - return: 64 byte array
 -}
salsaCore :: [Word8] -> Reader Config [Word8]
salsaCore bytes
    | length bytes /= 64 = error $ "[Salsa] Bad input size: " ++ show (length bytes)
    | otherwise = do
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
        return $ word32ArrayToWord8ArrayLE out


{-
 - bytes:   1024 byte array
 - ys:      (64 * i) byte array    [i=0..2*r-1]
 - return:  (64 * i) byte array    [i=0..2*r-1]
 -}
blockMixInner :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
blockMixInner bytes ys i = do
    -- B[i]: current input block
    let b = take 64 (drop (i*64) bytes)
    -- X: Y[i] from previous iteration
    let x = drop (length ys - 64) ys
    let t = zipWith xor x b
    out <- salsaCore t
    return $ ys ++ out

{-
 -  1. X = B[2 * r - 1]
 -
 -  2. for i = 0 to 2 * r - 1 do
 -       T = X xor B[i]
 -       X = Salsa (T)
 -       Y[i] = X
 -
 -  3. (Y[0], Y[2], ..., Y[2 * r - 2],
 -      Y[1], Y[3], ..., Y[2 * r - 1])
 -
 -
 -  vs:     (1024 * i) byte array  [i=0..n-1]
 -  return: (1024 * i) byte array
 -}
blockMix :: [Word8] -> Int -> Reader Config [Word8]
blockMix vs _ = do
    cfg <- ask
    let r = blockSize cfg
    -- Each blockMix call uses the result from the last call as B[]
    let bytes = drop (length vs - 128*r) vs
    let x = drop (64*(2*r-1)) bytes
    out <- foldlM (blockMixInner bytes) x [0..2*r-1]
    -- XXX: Exclude the starting value of 'x' from the output
    let ys = drop 64 out

    return $ vs ++ concat (
             [take 64 (drop (64*i)     ys) | i <- [0,2..2*r-2]] ++
             [take 64 (drop (64*(i+1)) ys) | i <- [0,2..2*r-1]])

{-
 - Not obvious that this is what you need to do from the RFC description...
 - https://www.rfc-editor.org/errata/eid6452
 -}
integerify :: [Word8] -> Int -> Int
integerify bytes n
    | length bytes /= 8 = error $ "[integerify] Bad input size: " ++ show (length bytes)
    | otherwise = do
        let b1 :: Word64 = fromIntegral $ head $ word8toWord32ArrayLE (take 4 bytes)
        let b2 :: Word64 = fromIntegral $ head $ word8toWord32ArrayLE (drop 4 bytes)
        let b3 :: Word64 = b1 .|. (b2 `shiftL` 32)
        fromIntegral $ b3 `mod` fromIntegral n


xorBlockMix :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
xorBlockMix vs x _ = do
    cfg <- ask
    let n = memoryCost cfg
    let r = blockSize cfg
    -- The next index from V[] to use is based of the 64-bit integer
    -- interpretation of the first 8 bytes in the last (64 byte) block of X.
    let jj = take 8 (drop ((2*r -1)*16*4) x)
    let j = integerify jj  n

    let v = take (128*r) (drop (128*r*j) vs)
    let t = zipWith xor x v

    out <- blockMix t 0
    -- Only return the new X value
    return $ drop (length out - 128*r) out


{-
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
 -
 -  bytes: 1024 byte array
 -}
romMix :: [Word8] -> Reader Config [Word8]
romMix bytes = do
    cfg <- ask
    let r = blockSize cfg
    let n = memoryCost cfg

    -- Step 1-2
    -- For N=2:
    --   V[0] = X
    --   V[1] = scryptBlockMix(V[0])
    --   X = scryptBlockMix(V[1])
    --
    -- Note: the final output block from scryptBlockMix is not part of V[]
    -- but it is used in step 3!
    let v0 = bytes
    out <- foldlM blockMix v0 [0..n-1]

    -- Step 3-4
    let vs = take (128*r*n) out
    let x = drop (128*r*n) out
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
 - https://raw.githubusercontent.com/viniciuschiele/Scrypt/refs/heads/master/src/Scrypt/ScryptEncoder.cs
 -
 - password:  [any] byte array
 - salt:      [any] byte array
 - return:    [dkLen] byte array
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

    -- New calculation of Pbkdf2 on the password but with b2 as the salt
    Pbkdf2.deriveKey password (concat bs) (derivedKeyLength cfg)
