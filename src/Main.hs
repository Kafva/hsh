{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Md5
import Sha1
import Template
import Types (Config(..))
import Util (word8ArrayToHexString, word8ArrayToHexArray)
import qualified Log

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
        "Select algorithm [md5,sha1]"
    ]


readMe :: Int -> Reader Config String
readMe number = do
    cfg <- ask
    return (algorithm cfg ++ (show number))

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
    runReaderT (Log.debug' "%s\n" (show opts)) opts

    let _ = runReaderT (readMe 77) opts

    -- Read from stdin
    input <- B.getContents
    let bytes :: [Word8] = B.unpack input

    runReaderT (Log.debug' "raw: %s\n" (word8ArrayToHexArray bytes 64)) opts

    case algorithm opts of
        "md5"  -> do
            let digest = runReader (Md5.hash bytes) opts
            -- runReaderT (Log.debug' "table: %s" $ show $(md5Table)) opts
            -- runReaderT (Log.debug' "raw input length %d byte(s)\n" (length bytes)) opts
            -- runReaderT (Log.debug' "output: %s\n" (word8ArrayToHexArray digest)) opts
            -- runReaderT (Log.debug' "output length %d byte(s)\n" (length digest)) opts

            putStrLn $ word8ArrayToHexString digest 16

        "sha1" -> do
            print $ Sha1.hash bytes

        alg ->
            putStrLn $ "Invalid algorithm: " ++ alg

    exitSuccess

