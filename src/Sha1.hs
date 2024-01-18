module Sha1 (hash) where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary
import Data.Binary (Word8)

hash :: [Char] -> [Word8]
hash inputData = do
    let byteString :: ByteStringLazy.ByteString = Binary.encode inputData
    ByteStringLazy.unpack byteString
