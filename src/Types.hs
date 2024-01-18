{-# LANGUAGE StrictData #-}

module Types (
    Config(..), -- constructor
    ConfigMonad,
    word8ArrayToHexString
) where

import Control.Monad.Reader
import Data.Binary (Word8)

import Numeric (showHex)
import Data.Char(toUpper)

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

