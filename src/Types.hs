{-# LANGUAGE StrictData #-}

module Types (
    Config(..), -- constructor
    ConfigMonad,
    Md5Digest,
    Sha1Digest,
    Sha256Digest,
    Sha1ArrayW,
    Sha256ArrayW,
    Block
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

type Md5Digest    = [Word32] -- 4 slots
type Sha1Digest   = [Word32] -- 5 slots
type Sha256Digest = [Word32] -- 8 slots
type Sha256ArrayW = [Word32] -- 64 slots
type Block        = [Word32] -- 16 slots
type Sha1ArrayW   = [Word32] -- 80 slots
