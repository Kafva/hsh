{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Md5
import Sha1
import Template
import Types (Config(..))
import qualified Log

import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import Data.Foldable (for_)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.Reader


defaultOptions :: Config
defaultOptions = Config {
    help = False,
    version = False,
    debug = False,
    algorithm = "md5"
}

usage :: IO ()
usage = do
    programName <- getProgName
    hPutStrLn stderr $ usageInfo ("usage: " ++ programName) options

-- OptDescr is a type that holds
-- Option {
--  [Char]        Short options
--  [String]      Long options
--  (ArgDescr a)  Descriptor (a Config -> IO Config function in our case)
--  String        Help text
-- }
-- By mapping to an `IO` function we can print directly in the handler.
options :: [OptDescr (Config -> IO Config)]
options = [
        Option ['V'] ["version"] (NoArg (\_ -> do
            programName <- getProgName
            putStrLn $ programName ++ " " ++ $(Template.programVersion)
            exitFailure
        )) "Show version",

        Option ['d'] ["debug"] (NoArg (\opt -> do
            return opt { debug = True }
        )) "Verbose logging",

        Option ['h'] ["help"] (NoArg (\_ -> do
            usage
            exitFailure
        )) "Print help information",

        Option ['a'] ["algorithm"] (ReqArg (\arg opt ->
            return opt { algorithm = arg }
        ) "algorithm")
        "Select algorithm [md5,sha1]"
    ]

main :: IO ()
main = do
    args <- getArgs
    -- `optionsFn` will hold the (Config -> IO Config) functions defined for each option
    let (optionsFn, _, errors) = getOpt RequireOrder options args

    -- Check for command line parsing errors
    {- HLINT ignore "Redundant bracket" -}
    {- HLINT ignore "Use null" -}
    when ((length errors) > 0) $ do
        for_ errors putStr
        exitFailure

    -- Apply default options
    opts <- foldl (>>=) (return defaultOptions) optionsFn

    -- Read from stdin
    -- input <- Prelude.getContents
    let input = ['a', 'b', 'c']

    case algorithm opts of
        "md5"  -> do
            let digest = Md5.hash input
            Log.debug' "input length %d bit(s)" (8*length input)
            Log.debug' "digest length %d bit(s)" (8*length digest)

        "sha1" -> do
            print $ Sha1.hash input

        alg ->
            putStrLn $ "Invalid algorithm: " ++ alg

    exitSuccess

