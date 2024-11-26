{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Md5
import Sha1
import Sha256
import Pbkdf2
import Hmac
import Template
import Types (Config(..), HashSignature)
import Util (word8ArrayToHexString, word8ArrayToHexArray, stringToInt, intToString)
import Log (debug', debug'')

import Control.Monad (unless)
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import Data.Foldable (for_)
import System.Exit (exitFailure, exitSuccess, die)
import Control.Monad.Reader (runReaderT, runReader)

import qualified Data.ByteString as BS
import Data.Word (Word8)

defaultOptions :: Config
defaultOptions = Config {
    help = False,
    version = False,
    debug = False,
    algorithm = "",
    innerAlgorithm = Sha1.hash,
    innerAlgorithmLength = 20,
    keySource = "",
    iterations = 512,
    derivedKeyLength = 64
}

usage :: IO ()
usage = do
    programName <- getProgName
    let header = "USAGE:\n" ++
                 programName ++ " [OPTIONS]\n\n" ++
                 "Calculate hashes etc. of input from stdin.\n\n" ++
                 "STDIN:\n" ++
                 "  Acts as the input stream for hash algorithms\n" ++
                 "  Acts as the input message for HMAC\n" ++
                 "  Acts as the salt for PBKDF2\n\n" ++
                 "OPTIONS:"
    hPutStrLn stderr $ usageInfo header options

stringToHashAlgorithm :: String -> (HashSignature, Int)
stringToHashAlgorithm s = case s of
     "md5" -> (Md5.hash, 16)
     "sha1" -> (Sha1.hash, 20)
     "sha224" -> (Sha256.hash224, 28)
     "sha256" -> (Sha256.hash256, 32)
     _ -> error ("Unknown hash algorithm: '" ++ s ++ "'")

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

        Option ['a'] ["algorithm"] (ReqArg (\arg opt -> do
            return opt { 
                algorithm = arg
            }
        ) "algorithm")
        "Select algorithm [md5,sha1,sha224,sha256,hmac,pbkdf2,scrypt]",

        Option ['H'] ["hash"] (ReqArg (\arg opt -> do
            let (hashFunction, hashLength) = stringToHashAlgorithm arg
            return opt { 
                innerAlgorithm = hashFunction, 
                innerAlgorithmLength = hashLength
            }
        ) "hash")
        "hmac,pbkdf2: Select underlying hash algorithm [md5,sha1,sha224,sha256]",

        Option ['k'] ["key"] (ReqArg (\arg opt ->
            return opt { keySource = arg }
        ) "stream")
        "hmac,pbkdf2: Key material input stream",

        Option ['i'] ["iterations"] (ReqArg (\arg opt ->
            return opt { iterations = stringToInt arg }
        ) "count")
        ("pbkdf2: Iterations to use for key derivation [default: " ++ intToString (iterations defaultOptions) ++ "]"),

        Option ['l'] ["length"] (ReqArg (\arg opt ->
            return opt { derivedKeyLength = stringToInt arg }
        ) "count")
        ("pbkdf2: Length of derived key to generate [default: " ++ intToString (derivedKeyLength defaultOptions) ++ " bytes]")
    ]

main :: IO ()
main = do
    args <- getArgs
    -- `optionsFn` will hold the (Config -> IO Config) functions defined for each option
    let (optionsFn, _, errors) = getOpt RequireOrder options args

    -- Check for command line parsing errors
    unless (null errors) $ do
        for_ errors putStr
        exitFailure

    -- Apply default options
    opts <- foldl (>>=) (return defaultOptions) optionsFn
    runReaderT (debug' "%s\n" (show opts)) opts

    -- Read from stdin
    input <- BS.getContents
    let bytes :: [Word8] = BS.unpack input

    runReaderT (debug'' "input [%d byte(s)]: %s \n"
                (length bytes)
                (word8ArrayToHexArray bytes 64)) opts

    case algorithm opts of
        s | s == "md5" || s == "sha1" || s == "sha224" || s == "sha256" -> do
            let (hashFunction, outLength) = stringToHashAlgorithm (algorithm opts)
            let digest = runReader (hashFunction bytes) opts
            putStrLn $ word8ArrayToHexString digest outLength

        s | s == "hmac" || s == "pbkdf2" -> do
            keyByteString <- if keySource opts == ""
                              then die "No key data provided"
                              else BS.readFile (keySource opts)
            let key = BS.unpack keyByteString
            runReaderT (debug'' "key [%d byte(s)]: %s\n"
                (length key)
                (word8ArrayToHexArray key (length key))) opts

            case algorithm opts of
                "hmac" -> do
                    let mac = runReader (Hmac.calculate bytes key) opts
                    putStrLn $ word8ArrayToHexString mac 32
                "pbkdf2" -> do
                    let derivedKey = runReader (Pbkdf2.deriveKey key bytes) opts
                    putStrLn $ word8ArrayToHexString derivedKey (2 * derivedKeyLength opts)
                _ -> putStrLn $ "Invalid algorithm: " ++ algorithm opts

        "scrypt" -> do
            putStrLn $ word8ArrayToHexString [0x0] 20

        alg ->
            if alg == ""
            then usage
            else putStrLn $ "Invalid algorithm: " ++ alg

    exitSuccess
