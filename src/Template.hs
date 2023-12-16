{-# LANGUAGE TemplateHaskell #-}

module Template (getVersion) where

import System.Process
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

getVersion = "0.1.0"
-- getVersion :: String -> Q Exp
-- getVersion = do
--   output <- runIO $ readProcess "awk" ["'/^version: /{print $2}'", "hsh.cabal"] []
--   liftData output


-- md5table :: [Byte]
-- md5table =  map (\i -> floor $ (2**32 * (abs $ sin $ i+1)))  [0..63]

