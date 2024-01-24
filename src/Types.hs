{-# LANGUAGE StrictData #-}

module Types (
    Config(..), -- constructor
    ConfigMonad,
    word8ArrayToHexArray,
    word8ArrayToHexString,
) where

import Control.Monad.Reader
import Data.Binary (Word8)

import Numeric (showHex)
import Data.Char(toLower)

-- A Monad stack that allows us to run both IO and read from the Config
type ConfigMonad a = ReaderT Config IO a

data Config = Config {
    help :: Bool,
    version :: Bool,
    debug :: Bool,
    algorithm :: String
} deriving Show

word8ToHexString :: String -> Word8 -> String
word8ToHexString prefix w = do
    let hexValue = map toLower (showHex w "")
    if length hexValue == 1
    then prefix ++ "0" ++ hexValue
    else prefix ++ hexValue

word8ArrayToHexArray :: [Word8] -> String
word8ArrayToHexArray [] = "[]"
word8ArrayToHexArray arr = do
    let s = concatMap ((++ ", ") . word8ToHexString "0x") arr
    "[" ++ take (length s - 2) s ++ "]"

word8ArrayToHexString :: [Word8] -> String
word8ArrayToHexString [] = ""
word8ArrayToHexString arr = concatMap (word8ToHexString "") arr

