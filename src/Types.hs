module Types (
    Word8,
    Word32,
    Int64
) where

import qualified Data.Binary as Binary
import qualified Data.Word (Word32)
import qualified Data.Int (Int64)

type Word8 = Binary.Word8
type Word32 = Data.Word.Word32
type Int64 = Data.Int.Int64

