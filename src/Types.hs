{-# LANGUAGE StrictData #-}

module Types (
    Word8,
    Word32,
    Word64,
    Config(..), -- constructor
    ConfigMonad
) where

import Control.Monad.Reader
import qualified Data.Binary as Binary
import qualified Data.Word (Word32, Word64)

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
