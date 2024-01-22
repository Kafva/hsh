{-# LANGUAGE TemplateHaskell #-}

module Md5 (hash) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary

import Data.Bits ((.&.), (.|.), complement, xor, shiftL, shiftR)
import Data.Binary (Word8, Word32, Word64)

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


-- The indices move over the 64 slots in a block
-- The reference implementation iterates over i=1..64
-- Most others use i=0..63, these are indices in the md5Table.
--
-- X is the current block, an array of 16 Word32 values (64 bytes)
--  X[0..16]
--
-- T is the precomputed md5table, an array of 64 Word32 values
--  T[0..64]
--
--
consumeBlock :: Digest -> Block -> Digest
consumeBlock digest blk = digest


digestNewA :: NewDigestSignature
digestNewA a b c d blk auxFunction i t s = do
    let newA = auxRound a b c d auxFunction blk i t s
    [newA, b, c, d]

digestNewB :: NewDigestSignature
digestNewB a b c d blk auxFunction i t s = do
    let newB = auxRound b c d a auxFunction blk i t s
    [a, newB, c, d]

digestNewC :: NewDigestSignature
digestNewC a b c d blk auxFunction i t s = do
    let newC = auxRound c d a b auxFunction blk i t s
    [a, b, newC, d]

digestNewD :: NewDigestSignature
digestNewD a b c d blk auxFunction i t s = do
    let newD = auxRound d a b c auxFunction blk i t s
    [a, b, c, newD]

auxRound :: Word32 -> Word32 -> Word32 -> Word32 ->
            (Word32 -> Word32 -> Word32 -> Word32) ->
            Block ->
            Int -> Int -> Int ->
            Word32
auxRound a b c d auxFunction blk i t s =
    shiftL (b + (a + (auxFunction b c d) + (blk!!i) + $(md5Table)!!t)) s

-- Call `f` with each item in the provided array as a separate argument
expandDigestArray :: (Word32 -> Word32 -> Word32 -> Word32 -> a) -> [Word32] -> a
expandDigestArray f [a, b, c, d] = f a b c d
expandDigestArray _ _ = error "Invalid argument: expected list with 4 items"

processIndex :: Digest -> Block -> Int -> Digest
processIndex digest blk i
    -- Round 1
    -- (i) block index: range(0,15,1)   [0..16]
    -- (t) table index: range(0,15,1)   [0..64]
    -- (s) shift by:
    --      newA (7)
    --      newB (12)
    --      newC (17)
    --      newD (22)
    | i < 16 = case (mod i 4) of
        0 -> expandDigestArray digestNewA digest blk auxF i i 7
        1 -> expandDigestArray digestNewB digest blk auxF i i 12
        2 -> expandDigestArray digestNewC digest blk auxF i i 17
        _ -> expandDigestArray digestNewD digest blk auxF i i 22
    -- Round 2
    | i < 32 = digest
    -- Round 3
    | i < 48 = digest
    -- Round 4
    | otherwise = digest


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
hash :: [Word8] -> [Word8]
hash bytes = do
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
    let startDigest = [0x67452301,
                       0xefcdab89,
                       0x98badcfe,
                       0x10325476]

    -- (4) Process message
    -- The blocks are in multiples of 16 byte words, i.e. the digest can be
    -- evenly fit over it.
    -- Run the round function with each auxiliary function as described in the
    -- RFC, updating one slot in the digest for each `auxRound` call.
    let digest = processIndex startDigest (blocks!!0) 0
    concatMap word32ToWord8Array digest
