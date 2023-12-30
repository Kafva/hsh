module Log (debug', info', err') where

import Control.Monad.Reader
import Types(Config(..), ConfigMonad)
import Text.Printf (printf, PrintfArg)

debugPrintf :: String -> PrintfArg a => a -> String
debugPrintf fmt = printf ("\x1b[94mDEBUG\x1b[0m: " ++ fmt ++ "\n")

infoPrintf :: String -> PrintfArg a => a -> String
infoPrintf fmt = printf ("\x1b[92mINFO\x1b[0m: " ++ fmt)

errPrintf :: String -> PrintfArg a => a -> String
errPrintf fmt = printf ("\x1b[91mERROR\x1b[0m: " ++ fmt ++ "\n")

debug' :: String -> PrintfArg a => a -> ConfigMonad()
debug' fmt args = do
    config <- ask
    if debug config
       then liftIO $ putStr $ debugPrintf fmt args
       else liftIO $ putStr ""

info' :: String -> PrintfArg a => a -> IO()
info' fmt args = putStr $ infoPrintf fmt args

err' :: String -> PrintfArg a => a -> IO()
err' fmt args = putStr $ errPrintf fmt args
