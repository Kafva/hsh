module Types (
    Byte,
    Word32,
    Int64

) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary
import qualified Data.Word (Word32)
import qualified Data.Int (Int64)

type Byte = Binary.Word8
type Word32 = Data.Word.Word32
type Int64 = Data.Int.Int64

