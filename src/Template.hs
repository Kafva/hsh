{- Compile time executed functions -}
module Template (programVersion,
                 md5Table,
                 sha256Table,
                 sha256InitialDigest) where

import System.Process (readProcess)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Float.RealFracMethods (int2Double)

import Data.Word (Word32)

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

primes :: [Double]
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
          59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109,
          113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179,
          181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241,
          251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311]

sha2Constant :: Double -> Double -> Integer -> Integer
sha2Constant p n hexcnt = do
    let rootN =  p ** (1/n)
    let decimalPart = rootN - (int2Double (floor rootN))

    -- Shift `hexcnt` digits to the left so that they no longer reside behind the
    -- decimal '.', note that we expand with 16 as the base to get the hex
    -- representation of each digit.
    floor $ decimalPart * (16^hexcnt)

{-
 -
 - Generate the constants used in SHA224-256,
 - These are derived from the decimal part (i.e. the digits after the decimal
 - point '.') of the cube root of the first 80 primes.
 - SHA224-256 use the first 32 bits while SHA384-512 use the first 64 bits of
 - this value (the current approach does not have enough precision to get all 64
 - bits correct).
 -
 - Example for '2':
 -  2**(1/3) - floor(2**(1/3)) == 0.2599210498948732
 -
 -  The hex expression for the decimal digits is: 428a2f98
 -      4  / 16**1 = 0.25
 -      2  / 16**2 = 0.0078125
 -      8  / 16**3 = 0.001953125
 -      10 / 16**4 = 0.000152587890625
 -      2  / 16**5 = 1.9073486328125e-06
 -      15 / 16**6 = 8.940696716308594e-07
 -      9  / 16**7 = 3.3527612686157227e-08
 -      8  / 16**8 = 1.862645149230957e-09
 -  sum(...) ~= 0.2599210...
 -
 - https://crypto.stackexchange.com/a/41501/95946
 -}
sha256Table :: Q Exp
sha256Table = lift $ map (\p -> sha2Constant p 3 8) primes

sha256InitialDigest :: Q Exp
sha256InitialDigest = lift $ map (\p -> sha2Constant p 2 8) (take 8 primes)
