{-# LANGUAGE TemplateHaskell #-}

module Sha256 (hash) where

import Control.Monad.Reader
import Data.Foldable (foldl', foldlM)
import Data.Binary (Word8, Word32)
import Data.Bits ((.&.), (.|.), complement, xor, rotateL, rotateR)
import Log (trace', trace'')
import Types (Config, Block)
import Util (padSha1Input,
             word8ArrayToHexArray,
             word8toWord32ArrayBE,
             word32ArrayToBlocks,
             word32ArrayToWord8ArrayBE,
             showSha256Digest)
import Template (sha256Table)

{-
 - https://www.ietf.org/rfc/rfc6234.txt
 -
 - SHA224-256 operates on
 -  * 32-bit words
 -  * 512-bit blocks
 -  * 32 byte digest
 -
 - SHA384-512 operate on
 -  * 64-bit words
 -  * 1024-bit blocks
 -  * 64 byte digest
 -
 -}
hash :: [Word8] -> Reader Config [Word8]
hash bytes = do
    -- * Pad the input (identical approach to SHA1)
    let paddedBytes = padSha1Input bytes
    blocks :: [Block] <- trace' "input: %s" (word8ArrayToHexArray paddedBytes 64) $
                         (word32ArrayToBlocks $ word8toWord32ArrayBE paddedBytes)


    let finalDigest :: [Word32] = $(sha256Table)

    trace' "output: %s" (word8ArrayToHexArray (word32ArrayToWord8ArrayBE finalDigest) 64) $
        word32ArrayToWord8ArrayBE finalDigest


    --return $ trace' "output: %s" (word8ArrayToHexArray table) $ table

