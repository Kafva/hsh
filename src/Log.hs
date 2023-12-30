module Log (debugPrintf, debug', errPrintf, err') where

import Types(Config)
import Text.Printf (printf, PrintfArg)

debugPrintf :: String -> PrintfArg a => a -> String
debugPrintf fmt = printf ("\x1b[34mDEBUG\x1b[0m: " ++ fmt ++ "\n")

errPrintf :: String -> PrintfArg a => a -> String
errPrintf fmt = printf ("\x1b[31mERROR\x1b[0m: " ++ fmt ++ "\n")

debug' :: String -> PrintfArg a => a -> IO()
debug' fmt args = putStr $ debugPrintf fmt args

err' :: String -> PrintfArg a => a -> IO()
err' fmt args = putStr $ errPrintf fmt args

