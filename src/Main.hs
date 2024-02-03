{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Md5
import Sha1
import Sha256
import Template
import Types (Config(..))
import Util (word8ArrayToHexString, word8ArrayToHexArray)
import Log (debug')

import GHC.Base (when)
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import Data.Foldable (for_)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.Reader

import qualified Data.ByteString as B
import Data.Word (Word8)

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
        "Select algorithm [md5,sha1,sha224,sha256]"
    ]

main :: IO ()
main = do
    args <- getArgs
    -- `optionsFn` will hold the (Config -> IO Config) functions defined for each option
    let (optionsFn, _, errors) = getOpt RequireOrder options args

    -- Check for command line parsing errors
    when ((length errors) > 0) $ do
        for_ errors putStr
        exitFailure

    -- Apply default options
    opts <- foldl (>>=) (return defaultOptions) optionsFn
    runReaderT (debug' "%s\n" (show opts)) opts

    -- Read from stdin
    input <- B.getContents
    let bytes :: [Word8] = B.unpack input

    runReaderT (debug' "raw input: %s\n" (word8ArrayToHexArray bytes 64)) opts

    case algorithm opts of
        "md5"  -> do
            let digest = runReader (Md5.hash bytes) opts
            putStrLn $ word8ArrayToHexString digest 16

        "sha1" -> do
            let digest = runReader (Sha1.hash bytes) opts
            putStrLn $ word8ArrayToHexString digest 20

        "sha224" -> do
            let digest = runReader (Sha256.hash224 bytes) opts
            putStrLn $ word8ArrayToHexString digest 32

        "sha256" -> do
            let digest = runReader (Sha256.hash256 bytes) opts
            putStrLn $ word8ArrayToHexString digest 32

        alg ->
            putStrLn $ "Invalid algorithm: " ++ alg

    exitSuccess
