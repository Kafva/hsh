module Log (debug, debug2, info, err) where

import Debug.Trace (trace)
import Text.Printf (printf,PrintfArg)
import qualified System.IO as IO

debugPrintf :: String -> PrintfArg a => a -> String
debugPrintf str arg = printf "\x1b[34mDEBUG\x1b[0m: %s %v" str arg

debug :: String -> PrintfArg a => a -> a
debug str arg = trace (debugPrintf str arg) arg

info :: String -> PrintfArg a => a -> a
info str arg = trace (printf "\x1b[32mINFO\x1b[0m: %s %v" str arg) arg

err :: String -> PrintfArg a => a -> a
err str arg = trace (printf "\x1b[31mERROR\x1b[0m: %s %v" str arg) arg

debug2 :: String -> PrintfArg a => a -> IO()
debug2 str arg = IO.putStrLn $ debugPrintf str arg
