module Scrypt (Scrypt.deriveKey) where

import Pbkdf2
import Data.Array
import Data.Binary (Word8, Word32, Word64)
import Control.Monad.Reader
import Data.Foldable (foldlM)
import Types (Config(..))
import Data.Bits ((.|.), xor, shiftL, shiftR)
import Util (word8toWord32ArrayLE, word32ArrayToWord8ArrayLE, word8ArrayToHexArray, word8toWord64ArrayLE)
import Log (trace', trace'')
import Control.Monad (forM, when)

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

        -- trace'' "[Salsa] in:%s\n               out: %s"
        --     (word8ArrayToHexArray bytes 4)
        --     (word8ArrayToHexArray (word32ArrayToWord8ArrayLE out) 4) $
        --     word32ArrayToWord8ArrayLE out


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

    -- q2 <- trace'' "out[%d]=%s" i (word8ArrayToHexArray out 4) $ ys ++ out
    -- return $ q2


    -- q <- trace'' "in[%d]=%s" i (word8ArrayToHexArray b 4) $ ys ++ out
    -- q2 <- trace'' "t[%d]=%s" i (word8ArrayToHexArray t 4) $ q
    -- q3 <- trace'' "x[%d]=%s" i (word8ArrayToHexArray x 4) $ q2
    -- return $ q3

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
 -  bytes:  1024 byte array
 -  vs:     (1024 * i) byte array  [i=0..n-1]
 -  return: (1024 * i) byte array
 -}
blockMix :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
blockMix bytes vs _
    -- TODO: hardcoded r
    | length bytes /= 128*8 = error $ "[blockMix] Bad input size: " ++ show (length bytes)
    | otherwise = do
        cfg <- ask
        let r = blockSize cfg
        -- Block count when intreprting bytes as split into 64 byte blocks
        let blockCount = 2*r

        let x = drop (64*(blockCount-1)) bytes
        out <- foldlM (blockMixInner bytes) x [0..blockCount-1]
        -- XXX: Exclude the starting value of 'x' from the output
        let ys = drop 64 out

        -- let ret = vs ++ concat (
        --             [take 64 (drop (64*i)     ys) | i <- [0,2..blockCount-2]] ++
        --             [take 64 (drop (64*(i+1)) ys) | i <- [0,2..blockCount-1]])
        --trace'' "blockMix[%d]=%s" idx (word8ArrayToHexArray (drop (length vs) ret) 4) $ ret
        --trace'' "blockMix[%d]=%s" idx (word8ArrayToHexArray ys 4) $ ret

        return $ vs ++ concat (
                 [take 64 (drop (64*i)     ys) | i <- [0,2..blockCount-2]] ++
                 [take 64 (drop (64*(i+1)) ys) | i <- [0,2..blockCount-1]])

{-
 - The RFC description of this is pretty meh...
 - https://www.rfc-editor.org/errata/eid6452
 -}
integerify :: [Word8] -> Int -> Int
integerify bytes n = do
    let b1 :: Word64 = fromIntegral $ head $ word8toWord32ArrayLE (take 4 bytes)
    let b2 :: Word64 = fromIntegral $ head $ word8toWord32ArrayLE (drop 4 bytes)
    let b3 :: Word64 = b1 .|. (b2 `shiftL` 32)
    fromIntegral $ b3 `mod` fromIntegral n



xorBlockMix :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
xorBlockMix vs x _
    | length vs /= 128*8*(2+1) = error $ "[xorBlockMix] Bad input size: " ++ show (length vs)
    | otherwise = do
        cfg <- ask
        let n = memoryCost cfg
        let r = blockSize cfg
        let i1 = take 8 (drop (4*(2*r-1)*16 + 64-8) x)
        let jj = integerify i1 1024
        j <- trace' "jj: %s" (word8ArrayToHexArray i1 8) $ jj `mod` n
        --j <- trace' "jj: %d" (length i1) $ jj `mod` n

        let v = take (128*r) (drop (128*r*j) vs)
        let t = zipWith xor x v
        --t2 <- trace'' "j=%d X=%s" j ((word8ArrayToHexArray (drop 0 x) 4))  $ t
        -- OK: V[0..2043]
        t2 <- trace'' "j=%d X[0]=%s" j ((word8ArrayToHexArray x 4))  $ t
        --t2 <- trace' "V[]=%s" ((word8ArrayToHexArray (drop (4*0) vs) 4))  $ t
        --t2 <- trace'' "j=%d XXX=%d" j (length vs)  $ t

        blockMix t2 [] 0


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
romMix bytes
    | length bytes /= 128*8 = error $ "[romMix] Bad input size: " ++ show (length bytes)
    | otherwise = do
        cfg <- ask
        let r = blockSize cfg
        let n = memoryCost cfg

        -- 1-2.
        --
        -- XXX: V[0] = X
        --      V[1] = scryptBlockMix(V[1-1])
        --      V[2] = scryptBlockMix(V[2-1])
        --      ...
        --      X = scryptBlockMix(V[n-1])
        --
        -- Note: the final output block from scryptBlockMix is not part of V[]
        -- but it is used in step 3!
        let v0 = bytes
        vs <- foldlM (blockMix bytes) v0 [0..n-1]

        -- 3-4.
        --let x = drop (length vs - 128*r) vs
        let x = take (128*r) vs
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

    -- integerify test:
    --let intin32 = [(i `mod` 255) :: Word32  | i <- [0..2048]]
    --let intin = word32ArrayToWord8ArrayLE intin32
    --let x = integerify (drop ((2*8 - 1) * 64) intin) 1024
    ----let x = integerify32 intin32 8 1024
    --trace'' "INTEGERIFY: %d\n%s" x (word8ArrayToHexArray intin 64) $ password


    -- dlv exec tests/bin/scrypt -- -d .testenv/key.dat .testenv/input.dat 8 8 1 64
    let r = blockSize cfg
    let p = parallelisationParam cfg
    let outLen = p * 128 * r

    -- Calculate a Pbkdf2 key for the provided password and salt
    b <- Pbkdf2.deriveKey password salt outLen

    -- Run romMix() over each 128*r bytes block of the derived key
    --bs <- forM [1..p] $ \i -> romMix (take (128 * r * i) b)
    --let b2 = concat bs
    b2 <- romMix (take (128 * r) b)

    -- New calculation of Pbkdf2 on the password but with b2 as the salt
    Pbkdf2.deriveKey password b2 (derivedKeyLength cfg)
