{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Md5
import Sha1
import Template
import qualified Log

import qualified System.IO as IO
import qualified System.Exit
import qualified System.Environment
import System.Console.GetOpt
import Data.Foldable (for_)
import Control.Monad (when)

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

usage :: IO ()
usage = do
    programName <- System.Environment.getProgName
    IO.hPutStrLn IO.stderr $ usageInfo ("usage: " ++ programName) options

-- TODO Use a Reader for the config...
--  https://stackoverflow.com/a/14179721/9033629

-- OptDescr is a type that holds
-- Option {
--  [Char]        Short options
--  [String]      Long options
--  (ArgDescr a)  Descriptor (a Flags -> IO Flags function in our case)
--  String        Help text
-- }
-- By mapping to an `IO` function we can print directly in the handler.
options :: [OptDescr (Flags -> IO Flags)]
options = [
        Option ['V'] ["version"] (NoArg (\_ -> do
            programName <- System.Environment.getProgName
            IO.putStrLn $ programName ++ " " ++ $(Template.programVersion)
            System.Exit.exitFailure
        )) "Show version",

        Option ['d'] ["debug"] (NoArg (\opt -> do
            return opt { debug = True }
        )) "Verbose logging",

        Option ['h'] ["help"] (NoArg (\_ -> do
            usage
            System.Exit.exitFailure
        )) "Print help information",

        Option ['a'] ["algorithm"] (ReqArg (\arg opt ->
            return opt { algorithm = arg }
        ) "algorithm")
        "Select algorithm [md5,sha1]"
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
    input <- Prelude.getContents

    case (algorithm opts) of
        "md5"  -> do
            let digest = Md5.hash input
            Log.debug' "input length %d bit(s)" (8*length input)
            Log.debug' "digest length %d bit(s)" (8*length digest)


        "sha1" -> do
            IO.putStrLn $ show $ Sha1.hash input

        alg ->
            IO.putStrLn $ "Invalid algorithm: " ++ alg

    System.Exit.exitSuccess

