module Main (main) where

import Md5
import Sha1
import Template

import qualified System.IO as IO
import qualified System.Exit
import qualified System.Environment
import qualified Data.ByteString.Lazy as LazyByteString
import System.Console.GetOpt
import Data.Foldable (for_)
import Control.Monad (when)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
-- Dictionary of CLI options
-- Note the use of 'deriving Show' to allow for the data to be
-- easily shown to stdout.
--
data Flags = Flags {
    help :: Bool,
    version :: Bool,
    algorithm :: String
} deriving Show

defaultOptions :: Flags
defaultOptions =  Flags {
    help = False,
    version = False,
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
            IO.putStrLn $ prg ++ " " ++ Template.getVersion
            System.Exit.exitSuccess
        )) "Show version",

        Option ['h'] ["help"] (NoArg (\_ -> do
            prg <- System.Environment.getProgName
            IO.hPutStrLn IO.stderr $ usageInfo ("usage: "++prg) options
            System.Exit.exitSuccess
        )) "Print help information",

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
    -- `optionsFn` will hold the (Flags -> IO Flags) functions defined for each option
    let (optionsFn, _, errors) = getOpt RequireOrder options args

    -- Check for command line parsing errors
    when ((length errors) > 0) $ do
        for_ errors IO.putStr
        System.Exit.exitFailure

    -- Apply default options
    opts <- foldl (>>=) (return defaultOptions) optionsFn

    -- Read from stdin
    input <- LazyByteString.getContents

    case (algorithm opts) of
        "md5"  -> IO.putStrLn $ show $ Md5.hash input
        "sha1" -> IO.putStrLn $ show $ Sha1.hash input
        _ -> IO.putStrLn "invalid algorithm"

    System.Exit.exitSuccess

