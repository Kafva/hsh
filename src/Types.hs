{-# LANGUAGE StrictData #-}

module Types (
    Config(..), -- constructor
    ConfigMonad,
    Md5Digest,
    Md5Block
) where

import Control.Monad.Reader
import Data.Binary (Word32)

-- A Monad stack that allows us to run both IO and read from the Config
type ConfigMonad a = ReaderT Config IO a

data Config = Config {
    help :: Bool,
    version :: Bool,
    debug :: Bool,
    algorithm :: String
} deriving Show

type Md5Digest = [Word32] -- 4 slots
type Md5Block = [Word32]  -- 16 slots

