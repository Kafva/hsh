{- Compile time executed functions -}
module Template (programVersion, md5Table, sha256Table) where

import System.Process (readProcess)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Word (Word32)

primes :: [Float] =
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
     67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
     139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
     223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283,
     293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379,
     383, 389, 397, 401, 409]


-- Extract version string from cabal manifest
programVersion :: Q Exp
programVersion = do
  output <- runIO $ readProcess "awk" ["/^version: /{printf $2}", "hsh.cabal"] ""
  liftData output



-- Generate the lookup table used for MD5 digest calculations
f :: Double -> Word32
f i = floor . (*2**32) . abs . sin $ i + 1

md5Table :: Q Exp
md5Table = lift $ map f [0..63]


sqrtFracWord32 :: Float -> Word32
sqrtFracWord32 p = 0 -- TODO

-- Generate the constants used in SHA256, the first 32 bits of the fractional
-- parts of the square root for the first 80 primes.
sha256Table :: Q Exp
sha256Table = lift $ map (\p -> sqrtFracWord32 p) primes
