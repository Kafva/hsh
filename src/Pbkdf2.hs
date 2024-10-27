module Pbkdf2 (deriveKey) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Types (Config)
import Log (trace')

deriveKey :: [Word8] -> Reader Config [Word8]
deriveKey bytes = do
    trace' "output: %s" "xd" $
        [0x0]


