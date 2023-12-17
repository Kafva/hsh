module Sha1 (hash) where

import Types

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary

hash :: [Char] -> [Word8]
hash inputData = do
    let byteString :: ByteStringLazy.ByteString = Binary.encode inputData
    ByteStringLazy.unpack byteString
