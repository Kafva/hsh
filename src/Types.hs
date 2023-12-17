module Types (
    Word8,
    Word32,
    Word64
) where

import qualified Data.Binary as Binary
import qualified Data.Word (Word32, Word64)

type Word8 = Binary.Word8
type Word32 = Data.Word.Word32
type Word64 = Data.Word.Word64

