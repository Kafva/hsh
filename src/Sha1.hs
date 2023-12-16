module Sha1 where

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Binary as Binary

type Byte = Binary.Word8

hash :: ByteStringLazy.ByteString -> [Byte]
hash inputData = []
