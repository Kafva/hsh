module Types (
    Word8,
    Word32,
    Word64,
    Config
) where

import qualified Data.Binary as Binary
import qualified Data.Word (Word32, Word64)

type Word8 = Binary.Word8
type Word32 = Data.Word.Word32
type Word64 = Data.Word.Word64

data Config = Config {
    help :: Bool,
    version :: Bool,
    debug :: Bool,
    algorithm :: String
} deriving Show
