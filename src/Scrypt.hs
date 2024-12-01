module Scrypt (deriveKey) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Types (Config(..))

deriveKey :: [Word8] -> [Word8] -> Reader Config [Word8]
deriveKey password salt = do
    return [0x0]
