{-
 - Trace functions, have not bothered with proper handling of variadic arguments
 -}
module Log (debug', trace', trace'') where

import Control.Monad.Reader
import Types(Config(..), ConfigMonad)
import Text.Printf (printf, PrintfArg)
import Debug.Trace

trace' :: String -> PrintfArg a => a -> b -> Reader Config b
trace' fmt arg toRun = do
    cfg <- ask
    if debug cfg
    then return $ trace (debugPrintf' fmt arg)toRun
    else return toRun

trace'' :: String -> PrintfArg a => a -> PrintfArg b => b -> c -> Reader Config c
trace'' fmt arg1 arg2 toRun = do
    cfg <- ask
    if debug cfg
    then return $ trace (debugPrintf'' fmt arg1 arg2) toRun
    else return toRun


debugPrintf' :: String -> PrintfArg a => a -> String
debugPrintf' fmt args = printf ("\x1b[94mDEBUG\x1b[0m: " ++ fmt) args

debugPrintf'' :: String -> PrintfArg a => a -> PrintfArg b => b -> String
debugPrintf'' fmt args = printf ("\x1b[94mDEBUG\x1b[0m: " ++ fmt) args

debug' :: String -> PrintfArg a => a -> ConfigMonad()
debug' fmt arg = do
    cfg <- ask
    if debug cfg
       then liftIO $ putStr $ debugPrintf' fmt arg
       else liftIO $ putStr ""
