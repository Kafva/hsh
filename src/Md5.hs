{-# LANGUAGE TemplateHaskell #-}

module Md5 (hash) where

import Template (md5Table)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL)
import Data.Binary (Word8, Word32, Word64)
import Log (debugPrintf, trace')
import Types (Md5Block, Md5Digest)
import Util (word8ArrayToHexArray,
             word8toWord32Array,
             word32ToWord8Array,
             word32ArrayToBlocks,
             word64ToWord8Array)

-- Type signature for each `digestNew` function
type NewDigestSignature = Word32 -> Word32 -> Word32 -> Word32 ->
                          Md5Block ->
                          (Word32 -> Word32 -> Word32 -> Word32) ->
                          Int -> Int -> Int ->
                          Md5Digest

type AuxiliaryFunctionSignature = Word32 -> Word32 -> Word32 -> Word32

{-
    Each of the auxiliary functions are defined to act over bits
    in each word and map 3 32-bit words onto 1.
-}
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
    [newA, b, c, d]

digestNewB :: NewDigestSignature
digestNewB a b c d blk auxFunction k s i = do
    let newB = auxRound b c d a auxFunction blk k s i
    [a, newB, c, d]

digestNewC :: NewDigestSignature
digestNewC a b c d blk auxFunction k s i = do
    let newC = auxRound c d a b auxFunction blk k s i
    [a, b, newC, d]

digestNewD :: NewDigestSignature
digestNewD a b c d blk auxFunction k s i = do
    let newD = auxRound d a b c auxFunction blk k s i
    [a, b, c, newD]

-- a = b + ((a + F(b,c,d) + X[k] + T[i]) <<< s)
auxRound :: Word32 -> Word32 -> Word32 -> Word32 ->
            (AuxiliaryFunctionSignature) ->
            Md5Block ->
            Int -> Int -> Int ->
            Word32
auxRound a b c d auxFunction blk k s i = do
    b + (rotateL (a + (auxFunction b c d) + (blk!!k) + $(md5Table)!!i) s)

-- Call `f` with each item in the provided array as a separate argument
expandDigestArray :: (Word32 -> Word32 -> Word32 -> Word32 -> a) -> [Word32] -> a
expandDigestArray f [a, b, c, d] = f a b c d
expandDigestArray _ _ = error "Invalid argument: expected list with 4 items"


-- The indices move over the 64 slots in a block
-- The reference implementation iterates over k=1..64
-- Most others use k=0..63, these are indices in the md5Table.
--
-- X is the current block, an array of 16 Word32 values (64 bytes)
--  X[0..16]
--
-- T is the precomputed md5table, an array of 64 Word32 values
--  T[0..64]
--
-- Note: the RFC description of rounds differs from the wikipedia description
-- but both are valid.
-- https://crypto.stackexchange.com/a/6320/95946
--
processIndexRecursive :: Md5Digest -> Md5Block -> Int -> Md5Digest
processIndexRecursive digest blk idx
  | idx == 63 = processIndex digest blk idx
  | otherwise = processIndexRecursive (processIndex digest blk idx) blk (idx + 1)


-- Helper to show the output from each step of processIndex
traceRound :: (NewDigestSignature) ->
              Md5Digest ->
              Md5Block ->
              (AuxiliaryFunctionSignature) ->
              Int ->
              Int ->
              Int ->
              Md5Digest
traceRound digestNewFunc digest blk auxFunc k s i  = do
    let out = expandDigestArray digestNewFunc digest blk auxFunc k s i
    let bytes = word8ArrayToHexArray $ concatMap word32ToWord8Array out
    trace' (debugPrintf "round = %s" bytes) True $ out

processIndex :: Md5Digest -> Md5Block -> Int -> Md5Digest
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
        0 -> expandDigestArray digestNewA digest blk auxF i 7 i
        1 -> expandDigestArray digestNewD digest blk auxF i 12 i
        2 -> expandDigestArray digestNewC digest blk auxF i 17 i
        _ -> expandDigestArray digestNewB digest blk auxF i 22 i
    -- Round 2
    -- (k) block index: range(1,15,5)   [0..16]
    -- (s) shift by:
    --      newA (5)
    --      newB (20)
    --      newC (14)
    --      newD (9)
    -- (i) table index: range(16,31,1)   [0..64]
    | i < 32 = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxG (mod (1 + i*5) 16) 5   i
        1 -> expandDigestArray digestNewD digest blk auxG (mod (1 + i*5) 16) 9   i
        2 -> expandDigestArray digestNewC digest blk auxG (mod (1 + i*5) 16) 14  i
        _ -> expandDigestArray digestNewB digest blk auxG (mod (1 + i*5) 16) 20  i
    -- Round 3
    | i < 48 = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxH (mod (5 + i*3) 16) 4   i
        1 -> expandDigestArray digestNewD digest blk auxH (mod (5 + i*3) 16) 11  i
        2 -> expandDigestArray digestNewC digest blk auxH (mod (5 + i*3) 16) 16  i
        _ -> expandDigestArray digestNewB digest blk auxH (mod (5 + i*3) 16) 23  i
    -- Round 4
    | otherwise = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxI (mod (i*7) 16) 6   i
        1 -> expandDigestArray digestNewD digest blk auxI (mod (i*7) 16) 10  i
        2 -> expandDigestArray digestNewC digest blk auxI (mod (i*7) 16) 15  i
        _ -> expandDigestArray digestNewB digest blk auxI (mod (i*7) 16) 21  i

{-
    Append a '1' bit and fill with '0' until the bit-length of the
    input adheres to:
        input % 512 == 448
    Or equivalently upon bytes:
        bytes % 64 == 56
-}
padZeroR :: [Word8] -> [Word8]
padZeroR bytes = do
    if (mod (length bytes) 64) /= 56
    then padZeroR $ bytes ++ [0x0]
    else bytes


processBlocks :: [Md5Block] -> Md5Digest -> Md5Digest
processBlocks blocks digest
    | length blocks == 0 = digest
    | otherwise = do
        let resultDigest = processIndexRecursive digest (blocks!!0) 0
        -- The digest registers are updated *per* block
        let updatedDigest = zipWith (+) digest resultDigest
        processBlocks (drop 1 blocks) updatedDigest

{-
    https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf
    https://www.ietf.org/rfc/rfc1321.txt

    Hash algorithms map a variable length bit-string onto a fixed length
    bit-string, 128 bits (16 bytes) in the case of MD5.
-}
hash :: [Word8] -> Bool -> [Word8]
hash bytes debug = do
    let unpaddedBitCount :: Word64 = fromIntegral (8 * length bytes)

                      -- * Add padding bits
    let paddedBytes = (padZeroR (bytes ++ [0b1000_0000])) ++
                      -- * Append the original length (in bits) as a 64-bit value
                      (word64ToWord8Array unpaddedBitCount)

    let blocks = trace' (debugPrintf "input: %s" (word8ArrayToHexArray (take 16 paddedBytes))) debug $
                 word32ArrayToBlocks $ word8toWord32Array paddedBytes

    -- * Set starting values
    -- The RFC uses Little-endian byte ordering for words,
    -- i.e. the LSB is at the lowest (first) address:
    --      0x11223344 ---> [0x44 0x33 0x22 0x11]
    --
    let startDigest = [0x6745_2301,
                       0xefcd_ab89,
                       0x98ba_dcfe,
                       0x1032_5476]

    let startBytes = concatMap word32ToWord8Array startDigest
    let msg = debugPrintf "start:  %s" (word8ArrayToHexArray startBytes)

    -- * Process message
    -- The blocks are in multiples of 16 byte words, i.e. the digest can be
    -- evenly fit over it.
    -- Run the round function with each auxiliary function as described in the
    -- RFC, updating one slot in the digest for each `auxRound` call.
    --
    -- The digest buffer should be copied at the start of each round and the
    -- result added to the previous round result
    let finalDigest = trace' msg debug $ processBlocks blocks startDigest

    concatMap word32ToWord8Array finalDigest
