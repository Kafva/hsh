{-# LANGUAGE TemplateHaskell #-}

module Md5 (hash) where

import Data.Bits ((.&.), (.|.), complement, xor, shiftL, shiftR)
import Data.Binary (Word8, Word32, Word64)
import Log (debugPrintf, trace')
import Types (word8ArrayToHexArray)

import Template (md5Table)

{-
    The output digest is comprised of 4 32-bit words (16 bytes)
    Field names: https://wiki.haskell.org/Name_clashes_in_record_fields
-}
type Digest = [Word32] -- 4 slots
type Block = [Word32]  -- 16 slots

-- Type signature for each `digestNew` function
type NewDigestSignature = Word32 -> Word32 -> Word32 -> Word32 ->
                          Block ->
                          (Word32 -> Word32 -> Word32 -> Word32) ->
                          Int -> Int -> Int ->
                          Digest

word32ToWord8Array :: Word32 -> [Word8]
word32ToWord8Array word = [fromIntegral word,
                           fromIntegral (shiftR word 8),
                           fromIntegral (shiftR word 16),
                           fromIntegral (shiftR word 24)]

word64ToWord8Array :: Word64 -> [Word8]
word64ToWord8Array word = [fromIntegral word,
                           fromIntegral (shiftR word 8),
                           fromIntegral (shiftR word 16),
                           fromIntegral (shiftR word 24),
                           fromIntegral (shiftR word 32),
                           fromIntegral (shiftR word 40),
                           fromIntegral (shiftR word 48),
                           fromIntegral (shiftR word 56)]

{-
    Each of the auxiliary functions are defined to act over bits
    in each word and map 3 32-bit words onto 1.
-}
auxF :: Word32 -> Word32 -> Word32 -> Word32
auxF x y z = (x .&. y) .|. ((complement x) .&. z)

auxG :: Word32 -> Word32 -> Word32 -> Word32
auxG x y z = (x .&. z) .|. (y .&. (complement z))

auxH :: Word32 -> Word32 -> Word32 -> Word32
auxH x y z = xor (xor x y) z

auxI :: Word32 -> Word32 -> Word32 -> Word32
auxI x y z = xor y (x .&. (complement z))

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
            (Word32 -> Word32 -> Word32 -> Word32) ->
            Block ->
            Int -> Int -> Int ->
            Word32
auxRound a b c d auxFunction blk k s i = do
    shiftL (b + (a + (auxFunction b c d) + (blk!!k) + $(md5Table)!!i)) s
    -- let sum1 = mod32add (blk!!k) ($(md5Table)!!i)
    -- let sum2 = mod32add a (auxFunction b c d)
    -- let sum3 = mod32add sum2 sum1
    -- shiftL (mod32add b sum3) s

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
-- but both should end up at the same result.
-- https://crypto.stackexchange.com/a/6320/95946
--
-- consumeBlock :: Digest -> Block -> Digest
-- consumeBlock startDigest blk = do
--     let resultDigest = processIndexRecursive startDigest blk 0
--     zipWith (+) startDigest resultDigest


mod32add :: Word32 -> Word32 -> Word32
mod32add a b = a + b

processIndexRecursive :: Digest -> Block -> Int -> Digest
processIndexRecursive digest blk k
    | k == 63 = processIndex digest blk 63
    -- Add result from first round to the starting value
    | k == 0 = zipWith (mod32add) digest $
               zipWith (mod32add) (processIndex digest blk k)
                           (processIndexRecursive digest blk (k + 1))
    -- Add result from next round to the previous round
    | otherwise = zipWith (mod32add) (processIndex digest blk k)
                              (processIndexRecursive digest blk (k + 1))

processIndex :: Digest -> Block -> Int -> Digest
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
        0 -> expandDigestArray digestNewA digest blk auxF i 7  i
        1 -> expandDigestArray digestNewB digest blk auxF i 12 i
        2 -> expandDigestArray digestNewC digest blk auxF i 17 i
        _ -> expandDigestArray digestNewD digest blk auxF i 22 i
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
        1 -> expandDigestArray digestNewB digest blk auxG (mod (1 + i*5) 16) 20  i
        2 -> expandDigestArray digestNewC digest blk auxG (mod (1 + i*5) 16) 14  i
        _ -> expandDigestArray digestNewD digest blk auxG (mod (1 + i*5) 16) 9   i
    -- Round 3
    | i < 48 = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxH (mod (5 + i*3) 16) 4   i
        1 -> expandDigestArray digestNewB digest blk auxH (mod (5 + i*3) 16) 23  i
        2 -> expandDigestArray digestNewC digest blk auxH (mod (5 + i*3) 16) 16  i
        _ -> expandDigestArray digestNewD digest blk auxH (mod (5 + i*3) 16) 11  i
    -- Round 4
    | otherwise = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxI (mod (i*7) 16) 6   i
        1 -> expandDigestArray digestNewB digest blk auxI (mod (i*7) 16) 10  i
        2 -> expandDigestArray digestNewC digest blk auxI (mod (i*7) 16) 15  i
        _ -> expandDigestArray digestNewD digest blk auxI (mod (i*7) 16) 21  i


word8ArrayToWord32 :: [Word8] -> Word32
word8ArrayToWord32 bytes =
    if length bytes /= 4
    then 0
    else (shiftL (fromIntegral $ bytes!!0) 24) .|.
         (shiftL (fromIntegral $ bytes!!1) 16) .|.
         (shiftL (fromIntegral $ bytes!!2) 8) .|.
         fromIntegral (bytes!!3)

-- Split the given array of bytes into a list of 32 byte entries
-- Returns an empty list if the list is not evenly divisible
word8toWord32Array :: [Word8] -> [Word32]
word8toWord32Array [] = []
word8toWord32Array arr = do
    if mod (length arr) 4 /= 0
    then []
    else word8ArrayToWord32 (take 4 arr) : word8toWord32Array (drop 4 arr)


word32ArrayToBlocks :: [Word32] -> [Block]
word32ArrayToBlocks [] = []
word32ArrayToBlocks arr = do
    if mod (length arr) 16 /= 0
    then []
    else
        (take 16 arr)
        : word32ArrayToBlocks (drop 16 arr)


padZeroR :: [Word8] -> [Word8]
padZeroR bytes = do
    if (mod (length bytes) 64) /= 56
    then padZeroR $ bytes ++ [0x0]
    else bytes

{-
    https://www.rfc-editor.org/rfc/pdfrfc/rfc1321.txt.pdf
    https://www.ietf.org/rfc/rfc1321.txt

    Hash algorithms map a variable length bit-string onto a fixed length
    bit-string, 128 bits (16 bytes) in the case of MD5.
-}
hash :: [Word8] -> Bool -> [Word8]
hash bytes debug = do
    -- (1) Add padding bits
    -- Append a '1' bit and fill with '0' until the bit-length of the
    -- input adheres to:
    --     input % 512 == 448
    -- Or equivalently upon bytes:
    --     bytes % 64 == 56
    --
    -- (2) Append length
    -- Append the 64 bit representation of the original length (in bits)
    let unpaddedBitCount :: Word64 = fromIntegral (8 * length bytes)
    let paddedBytes = (padZeroR (bytes ++ [0b1000_0000])) ++
                       word64ToWord8Array unpaddedBitCount

    let blocks = word32ArrayToBlocks $ word8toWord32Array paddedBytes

    -- (3) Set starting values
    --
    -- The RFC uses big-endian byte ordering, i.e. the MSB is at the highest
    -- address, i.e. 0x11223344 ---> [0x44 0x33 0x22 0x11]
    --
    let startDigest = [0x6745_2301,
                       0xefcd_ab89,
                       0x98ba_dcfe,
                       0x1032_5476]

    let startBytes = concatMap word32ToWord8Array startDigest
    let msg = debugPrintf "start:  %s" (word8ArrayToHexArray startBytes)

    -- (4) Process message
    -- The blocks are in multiples of 16 byte words, i.e. the digest can be
    -- evenly fit over it.
    -- Run the round function with each auxiliary function as described in the
    -- RFC, updating one slot in the digest for each `auxRound` call.
    --
    --
    -- The digest buffer should be copied at the start of each round and the
    -- result added to the previous round result
    let resultDigest = trace' msg debug $
                       processIndexRecursive startDigest (blocks!!0) 0
    concatMap word32ToWord8Array resultDigest
