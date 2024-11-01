{- HLINT ignore "Eta reduce" -}
{- Trace functions, no handling of variadic arguments -}
module Log (debug', debug'', trace', trace'') where

import Control.Monad.Reader
import Types(Config(..), ConfigMonad)
import Text.Printf (printf, PrintfArg)
import Debug.Trace

-- One argument...
trace' :: String -> PrintfArg a => a -> b -> Reader Config b
trace' fmt arg toRun = do
    cfg <- ask
    if debug cfg
    then return $ trace (debugPrintf' fmt arg) toRun
    else return toRun

-- Two arguments...
trace'' :: String -> PrintfArg a => a -> PrintfArg b => b -> c -> Reader Config c
trace'' fmt arg1 arg2 toRun = do
    cfg <- ask
    if debug cfg
    then return $ trace (debugPrintf'' fmt arg1 arg2) toRun
    else return toRun


-- One argument...
debugPrintf' :: String -> PrintfArg a => a -> String
debugPrintf' fmt args = printf ("\x1b[94mDEBUG\x1b[0m: " ++ fmt) args

-- Two arguments...
debugPrintf'' :: String -> PrintfArg a => a -> PrintfArg b => b -> String
debugPrintf'' fmt arg1 arg2 = printf ("\x1b[94mDEBUG\x1b[0m: " ++ fmt) arg1 arg2

debug' :: String -> PrintfArg a => a -> ConfigMonad()
debug' fmt arg = do
    cfg <- ask
    if debug cfg
       then liftIO $ putStr $ debugPrintf' fmt arg
       else liftIO $ putStr ""

debug'' :: String -> PrintfArg a => a -> PrintfArg b => b -> ConfigMonad()
debug'' fmt arg1 arg2 = do
    cfg <- ask
    if debug cfg
       then liftIO $ putStr $ debugPrintf'' fmt arg1 arg2
       else liftIO $ putStr ""

