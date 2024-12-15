{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Md5
import Sha1
import Sha256
import Hmac
import Pbkdf2
import Scrypt
import Template
import Types (Config(..), HashSignature)
import Util (word8ArrayToHexString, word8ArrayToHexArray, stringToInt, intToString)
import Log (debug', debug'')

import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import Data.Foldable (for_)
import System.Exit (exitFailure, exitSuccess, die)
import Control.Monad.Reader (runReaderT, runReader)
import Control.Monad (unless)

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits ((.&.))

defaultOptions :: Config
defaultOptions = Config {
    help = False,
    version = False,
    debug = False,
    algorithm = "",
    innerAlgorithm = Sha1.hash,
    innerAlgorithmLength = 20,
    keySource = "",
    iterations = 1,
    derivedKeyLength = 64,
    memoryCost = 32768,
    blockSize = 8,
    parallelisationParam = 1,
    enableThreads = False
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
                 "  Acts as the salt for PBKDF2 and SCRYPT\n\n" ++
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
        ("pbkdf2,scrypt: Length of derived key to generate [default: " ++ intToString (derivedKeyLength defaultOptions) ++ " bytes]"),

        Option ['N'] ["memory-cost"] (ReqArg (\arg opt ->
            return opt { memoryCost = stringToInt arg }
        ) "N")
        ("scrypt: CPU/Memory cost parameter [default: " ++ intToString (memoryCost defaultOptions) ++ "]"),

        Option ['p'] ["parallelisation"] (ReqArg (\arg opt ->
            return opt { parallelisationParam = stringToInt arg }
        ) "p")
        ("scrypt: Parallelisation parameter [default: " ++ intToString (parallelisationParam defaultOptions) ++ "]"),

        Option ['r'] ["block-size"] (ReqArg (\arg opt ->
            return opt { blockSize = stringToInt arg }
        ) "r")
        ("scrypt: Block size parameter [default: " ++ intToString (blockSize defaultOptions) ++ " bytes]"),

        Option ['T'] ["enable-threads"] (NoArg (\opt -> do
            return opt { enableThreads = True }
        ))
        ("pbkdf2,scrypt: Use threaded backend [default: " ++ show (enableThreads defaultOptions) ++ "]")

    ]

main :: IO ()
main = do
    args <- getArgs
    -- `options` will hold the (Config -> IO Config) functions defined for each option
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
    -- let bytes = [0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]

    runReaderT (debug'' "input [%d byte(s)]: %s \n"
                (length bytes)
                (word8ArrayToHexArray bytes 64)) opts

    case algorithm opts of
        s | s == "md5" || s == "sha1" || s == "sha224" || s == "sha256" -> do
            let (hashFunction, outLength) = stringToHashAlgorithm (algorithm opts)
            let digest = runReader (hashFunction bytes) opts
            putStrLn $ word8ArrayToHexString digest outLength

        s | s == "hmac" || s == "pbkdf2" || s == "scrypt" -> do
            keyByteString <- if keySource opts == ""
                              then die "ERROR: No key data provided"
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
                    derivedKey <- runReaderT (Pbkdf2.deriveKeyIO key bytes (derivedKeyLength opts)) opts
                    putStrLn $ word8ArrayToHexString derivedKey (2 * derivedKeyLength opts)
                "scrypt" -> do
                    unless (iterations opts == 1) $ die
                        "ERROR: Pbkdf2 iterations should be set to 1 for Scrypt"
                    -- A power of 2 has exactly one '1' in binary.
                    unless ((memoryCost opts == 1) ||
                            ((memoryCost opts .&. (memoryCost opts - 1)) == 0)) $ die
                        ("ERROR: N=" ++ show (memoryCost opts) ++ " must be a power of 2")

                    let maxP = ((2^(32::Integer)-1) * 32) `div` (128 * blockSize opts)
                    unless (maxP > parallelisationParam opts) $ die
                        ("ERROR: p=" ++ show (parallelisationParam opts) ++ " too large")

                    let derivedKey = runReader (Scrypt.deriveKey key bytes) opts
                    putStrLn $ word8ArrayToHexString derivedKey (length derivedKey)
                _ -> putStrLn $ "Invalid algorithm: " ++ algorithm opts

        alg ->
            if alg == ""
            then usage
            else putStrLn $ "Invalid algorithm: " ++ alg

    exitSuccess
