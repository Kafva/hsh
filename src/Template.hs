{- Compile time executed functions -}
module Template (programVersion, md5Table) where

import System.Process (readProcess)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
md5Table = lift (map f [0..63])

-- Generate the constants used in SHA256, the square-root of the first 80 prime
-- numbers.
-- sha256Table :: Q Exp
-- sha256Table = do
--     lift(


--     ) [0..80] :: [Word32]
