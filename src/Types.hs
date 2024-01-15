{-# LANGUAGE StrictData #-}

module Types (
    Word8,
    Word32,
    Word64,
    Config(..), -- constructor
    ConfigMonad,
    word8ArrayToHexString
) where

import Control.Monad.Reader
import qualified Data.Binary as Binary
import qualified Data.Word (Word32, Word64)

import Numeric (showHex)
import Data.Char(toUpper)

type Word8 = Binary.Word8
type Word32 = Data.Word.Word32
type Word64 = Data.Word.Word64


-- A Monad stack that allows us to run both IO and read from the Config
type ConfigMonad a = ReaderT Config IO a

data Config = Config {
    help :: Bool,
    version :: Bool,
    debug :: Bool,
    algorithm :: String
} deriving Show


word8ToHexString :: Word8 -> String
word8ToHexString w = do
    let hexValue = map toUpper (showHex w "")
    if length hexValue == 1
    then "0x0" ++ hexValue
    else "0x" ++ hexValue

word8ArrayToHexString :: [Word8] -> String
word8ArrayToHexString [] = "[]"
word8ArrayToHexString arr = do
    let s = concatMap ((++ ", ") . word8ToHexString) arr
    "[" ++ take (length s - 2) s ++ "]"

