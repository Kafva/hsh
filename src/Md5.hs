{-# LANGUAGE TemplateHaskell #-}

module Md5 (hash) where

import Control.Monad.Reader
import Template (md5Table)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL)
import Data.Binary (Word8, Word32)
import Log (trace', trace'')
import Data.Foldable (foldlM)
import Types (Config, Block, Md5Digest)
import Util (word8ArrayToHexArray,
             word8toWord32ArrayLE,
             word32ArrayToWord8ArrayLE,
             word32ArrayToBlocks,
             padMd5Input,
             showDigestArray)

-- Type signature for each `digestNew` function
type NewDigestSignature = Word32 -> Word32 -> Word32 -> Word32 ->
                          Block ->
                          (Word32 -> Word32 -> Word32 -> Word32) ->
                          Int -> Int -> Int ->
                          Reader Config Md5Digest

type AuxiliaryFunctionSignature = Word32 -> Word32 -> Word32 -> Word32

auxF :: AuxiliaryFunctionSignature
auxF x y z = (x .&. y) .|. ((complement x) .&. z)

auxG :: AuxiliaryFunctionSignature
auxG x y z = (x .&. z) .|. (y .&. (complement z))

auxH :: AuxiliaryFunctionSignature
auxH x y z = xor (xor x y) z

auxI :: AuxiliaryFunctionSignature
auxI x y z = xor y (x .|. (complement z))

digestNewA :: NewDigestSignature
digestNewA a b c d blk auxFunction k s i = do
    let newA = auxRound a b c d auxFunction blk k s i
    let newDigest = [newA, b, c, d]
    trace'' "ABCD [i=%d]: %s" i (showDigestArray newDigest 16) $ newDigest

digestNewB :: NewDigestSignature
digestNewB a b c d blk auxFunction k s i = do
    let newB = auxRound b c d a auxFunction blk k s i
    let newDigest = [a, newB, c, d]
    trace'' "BCDA [i=%d]: %s" i (showDigestArray newDigest 16) $ newDigest


digestNewC :: NewDigestSignature
digestNewC a b c d blk auxFunction k s i = do
    let newC = auxRound c d a b auxFunction blk k s i
    let newDigest = [a, b, newC, d]
    trace'' "CDAB [i=%d]: %s" i (showDigestArray newDigest 16) $ newDigest

digestNewD :: NewDigestSignature
digestNewD a b c d blk auxFunction k s i = do
    let newD = auxRound d a b c auxFunction blk k s i
    let newDigest = [a, b, c, newD]
    trace'' "DABC [i=%d]: %s" i (showDigestArray newDigest 16) $ newDigest

{-
 - a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s)
 -
 - X is the current block, an array of 16 Word32 values (64 bytes)
 -      X[0..16]
 - T is the precomputed md5table, an array of 64 Word32 values
 -     T[0..64]
 -}
auxRound :: Word32 -> Word32 -> Word32 -> Word32 ->
            (AuxiliaryFunctionSignature) ->
            Block ->
            Int -> Int -> Int ->
            Word32
auxRound a b c d auxFunction blk k s i = do
    b + (rotateL (a + (auxFunction b c d) + (blk!!k) + $(md5Table)!!i) s)

-- Call `f` with each item in the provided array as a separate argument
expandDigestArray :: (Word32 -> Word32 -> Word32 -> Word32 -> a) ->
                     [Word32] ->
                     a
expandDigestArray f [a, b, c, d] = f a b c d
expandDigestArray _ _ = error "Invalid argument: expected list with 4 items"

getK :: Int -> Int
getK i | i < 16 = i
       | i < 32 = mod (1 + i*5) 16
       | i < 48 = mod (5 + i*3) 16
       | otherwise = mod (i*7) 16

processIndex :: Md5Digest -> Block -> Int -> Reader Config Md5Digest
processIndex digest blk i
    -- Round 1
    -- (k) block index: range(0,15,1)   [0..16]
    -- (s) shift by:
    --      newA (7)
    --      newB (12)
    --      newC (17)
    --      newD (22)
    -- (i) table index: range(0,15,1)   [0..64]
    | i < 16 = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxF (getK i) 7  i
        1 -> expandDigestArray digestNewD digest blk auxF (getK i) 12 i
        2 -> expandDigestArray digestNewC digest blk auxF (getK i) 17 i
        _ -> expandDigestArray digestNewB digest blk auxF (getK i) 22 i
    -- Round 2
    -- (k) block index: range(1,15,5)   [0..16]
    -- (s) shift by:
    --      newA (5)
    --      newB (20)
    --      newC (14)
    --      newD (9)
    -- (i) table index: range(16,31,1)   [0..64]
    | i < 32 = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxG (getK i) 5   i
        1 -> expandDigestArray digestNewD digest blk auxG (getK i) 9   i
        2 -> expandDigestArray digestNewC digest blk auxG (getK i) 14  i
        _ -> expandDigestArray digestNewB digest blk auxG (getK i) 20  i
    -- Round 3
    | i < 48 = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxH (getK i) 4   i
        1 -> expandDigestArray digestNewD digest blk auxH (getK i) 11  i
        2 -> expandDigestArray digestNewC digest blk auxH (getK i) 16  i
        _ -> expandDigestArray digestNewB digest blk auxH (getK i) 23  i
    -- Round 4
    | otherwise = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxI (getK i) 6   i
        1 -> expandDigestArray digestNewD digest blk auxI (getK i) 10  i
        2 -> expandDigestArray digestNewC digest blk auxI (getK i) 15  i
        _ -> expandDigestArray digestNewB digest blk auxI (getK i) 21  i

processBlock :: Md5Digest -> Block -> Reader Config Md5Digest
processBlock digest block = do
        resultDigest <- foldlM (\d idx -> processIndex d block idx) digest [0..63]
        -- The digest registers are updated *per* block
        return $ zipWith (+) digest resultDigest

{-
 - https://www.ietf.org/rfc/rfc1321.txt
 -
 - Note: the RFC description of rounds differs from the wikipedia description
 - but both are valid.
 - https://crypto.stackexchange.com/a/6320/95946
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    -- * Pad the input to be a multiple of the block size (16 bytes)
    let paddedBytes = padMd5Input bytes
    blocks <-  trace' "input: %s" (word8ArrayToHexArray paddedBytes 64) $
               (word32ArrayToBlocks $ word8toWord32ArrayLE paddedBytes)

    -- * Set starting values
    -- The RFC uses Little-endian byte ordering for words,
    -- i.e. the LSB is at the lowest (first) address:
    --      0x11223344 ---> [0x44 0x33 0x22 0x11]
    --
    let startDigest = [0x6745_2301,
                       0xefcd_ab89,
                       0x98ba_dcfe,
                       0x1032_5476]

    -- * Process each block
    -- Run the round function with each auxiliary function as described in the
    -- RFC, updating one slot in the digest for each `digestNew` call.
    finalDigest <- foldlM processBlock startDigest blocks

    trace' "output: %s" (showDigestArray finalDigest 16) $
        word32ArrayToWord8ArrayLE finalDigest
