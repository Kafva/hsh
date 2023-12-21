module Log (debug, debug', info, info', err, err') where

import Debug.Trace (trace)
import Text.Printf (printf,PrintfArg)
import qualified System.IO as IO

debugPrintf :: String -> PrintfArg a => a -> String
debugPrintf fmt args = printf ("\x1b[34mDEBUG\x1b[0m: " ++ fmt) args

infoPrintf :: String -> PrintfArg a => a -> String
infoPrintf fmt args = printf ("\x1b[32mINFO\x1b[0m: " ++ fmt) args

errPrintf :: String -> PrintfArg a => a -> String
errPrintf fmt args = printf ("\x1b[31mERROR\x1b[0m: " ++ fmt) args

debug :: String -> PrintfArg a => a -> a
debug fmt args = trace (debugPrintf fmt args) args

debug' :: String -> PrintfArg a => a -> IO()
debug' fmt args = IO.putStrLn $ debugPrintf fmt args

info :: String -> PrintfArg a => a -> a
info fmt args = trace (infoPrintf fmt args) args

info' :: String -> PrintfArg a => a -> IO()
info' fmt args = IO.putStrLn $ infoPrintf fmt args

err :: String -> PrintfArg a => a -> a
err fmt args = trace (errPrintf fmt args) args

err' :: String -> PrintfArg a => a -> IO()
err' fmt args = IO.putStrLn $ errPrintf fmt args

