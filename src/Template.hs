{-
    Compile time executed functions
-}
module Template (programVersion, md5Table) where

import qualified System.Process
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Extract version string from cabal manifest
programVersion :: Q Exp
programVersion = do
  output <- runIO $ System.Process.readProcess
                    "awk" ["/^version: /{printf $2}", "hsh.cabal"] ""
  liftData output

-- Generate the lookup table used for MD5 digest calculations
md5Table :: Q Exp
md5Table = do
    lift (
        map (\i ->
            floor . (*2**32) . abs . sin $ (i :: Double) + 1
        )
        [0..63] :: [Int])
