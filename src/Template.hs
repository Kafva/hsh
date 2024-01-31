{- Compile time executed functions -}
module Template (programVersion, md5Table, sha256Table) where

import System.Process (readProcess)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Word (Word32, Word64)
import Data.Ratio
import Data.Bits (rotateL, rotateR)

primes :: [Integer] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
                       59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109,
                       113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179,
                       181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241,
                       251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311]


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


word64ToWord32ArrayBE :: Word64 -> [Word32]
word64ToWord32ArrayBE word = [fromIntegral (rotateR word 32),
                              fromIntegral word]


cubeRootFracWord32 :: Integer -> Word32
cubeRootFracWord32 p = do
    let cubeRoot = fromIntegral p ** (1 / 3.0)
    let d = denominator (toRational cubeRoot)
    (word64ToWord32ArrayBE (fromIntegral d))!!0


-- Generate the constants used in SHA256, the first 32 bits of the fractional
-- parts of the square root for the first 80 primes.
sha256Table :: Q Exp
sha256Table | (length primes) /= 64 = error "Incorrect length of primes array"
            | otherwise = lift $ map (\p -> cubeRootFracWord32 p) primes
