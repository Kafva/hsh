module Log (debug', trace') where

import Control.Monad.Reader
import Types(Config(..), ConfigMonad)
import Text.Printf (printf, PrintfArg)
import Debug.Trace

-- trace' :: forall a. String -> Bool -> a -> a
-- trace' str enabled a =
--     if enabled
--     then trace str $ a
--     else a

-- trace'' :: String -> Reader Config Bool
-- trace'' str = do
--     cfg <- ask
--     if (debug cfg)
--     then return $ trace str $ (debug cfg)
--     else return (debug cfg)


trace' :: String -> PrintfArg a => a -> b -> Reader Config b
trace' fmt args b = do
    cfg <- ask
    if (debug cfg)
    then return $ trace (debugPrintf fmt args) $ b
    else return b

-- myTrace :: Show a => a -> a
-- myTrace x = unsafePerformIO $ do
--   print x
--   pure x

-- trace'' :: String -> PrintfArg a => a -> ConfigMonad()
-- trace'' fmt args = do
--     cfg <- ask
--     if (debug cfg)
--     then return $ trace (debugPrintf fmt args) $ (debug cfg)
--     else return (debug cfg)



debugPrintf :: String -> PrintfArg a => a -> String
debugPrintf fmt args = printf ("\x1b[94mDEBUG\x1b[0m: " ++ fmt) args

debug' :: String -> PrintfArg a => a -> ConfigMonad()
debug' fmt args = do
    config <- ask
    if debug config
       then liftIO $ putStr $ debugPrintf fmt args
       else liftIO $ putStr ""
