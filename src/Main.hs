module Main (main) where

import Md5
import Sha1

import qualified System.IO as IO
import qualified System.Exit
import qualified System.Environment
import qualified Data.ByteString.Lazy as LazyByteString
import System.Console.GetOpt
import Data.Foldable (for_)
import Control.Monad (when)

programVersion :: String
programVersion = "0.1.0"

-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
-- Dictionary of CLI options
-- Note the use of 'deriving Show' to allow for the data to be
-- easily shown to stdout.
--
data Flags = Flags {
    help :: Bool,
    version :: Bool,
    debug :: Bool,
    algorithm :: String
} deriving Show

defaultOptions :: Flags
defaultOptions =  Flags {
    help = False,
    version = False,
    debug = False,
    algorithm = "md5"
}

-- OptDescr is a type that holds
-- Option {
--  [Char]                   Short options
--  [String]                 Long options
--  (ArgDescr a)             Descriptor (a Flags -> IO Flags function in our case)
--  String                   Help text
-- }
-- By mapping to an `IO` function we can print stuff directly in the handler.
options :: [OptDescr (Flags -> IO Flags)]
options = [
        Option ['V'] ["version"] (NoArg (\_ -> do
            prg <- System.Environment.getProgName
            IO.putStrLn $ prg ++ " " ++ programVersion
            System.Exit.exitSuccess
        )) "Show version",

        Option ['h'] ["help"] (NoArg (\_ -> do
            prg <- System.Environment.getProgName
            IO.hPutStrLn IO.stderr $ usageInfo ("usage: "++prg) options
            System.Exit.exitSuccess
        )) "Print help information",

        Option ['d'] ["debug"] (NoArg (\opt ->
            return opt { debug = True }
        )) "Print debug output",

        Option ['a'] ["algorithm"] (ReqArg (\arg opt ->
            -- Note that `ReqArg` has two arguments for its function
            -- `read` will perform string->int conversion
            return opt { algorithm = read arg }
        ) "ALG")
        "Select algorithm (md5,sha1)"
    ]


main :: IO ()
main = do
    args <- System.Environment.getArgs
    -- `actions` will hold the (Flags -> IO Flags) functions defined for each flag
    let (actions, _, errors) = getOpt RequireOrder options args

    -- Print command line parsing errors and exit if any occured
    when ((length errors) > 0) $ do
        for_ errors IO.putStr
        System.Exit.exitFailure

    -- With `foldl`, we apply the bind operator to each function in `actions`
    opts <- foldl (>>=) (return defaultOptions) actions

    -- Access to fields: 'object.field' -> 'field object'
    when (debug opts) $ putStrLn $ show opts

    -- Reads from stdin (does not wait when no input is given)
    input <- LazyByteString.getContents

    case (algorithm opts) of
        "md5"  -> IO.putStrLn $ show $ Md5.hash input
        "sha1" -> IO.putStrLn $ show $ Sha1.hash input
        _ -> IO.putStrLn "invalid algorithm"

    System.Exit.exitSuccess

