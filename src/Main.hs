{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Md5
import Sha1
import Sha256
import Pbkdf2
import Hmac
import Template
import Types (Config(..))
import Util (word8ArrayToHexString, word8ArrayToHexArray, stringToInt, intToString)
import Log (debug', debug'')

import Control.Monad (unless)
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import Data.Foldable (for_)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.Reader (runReaderT, runReader)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)

defaultOptions :: Config
defaultOptions = Config {
    help = False,
    version = False,
    debug = False,
    algorithm = "",
    saltSource = "",
    macKeySource = "",
    iterations = 1,
    derivedKeyLength = 4
}

defaultSalt :: String
defaultSalt = "abcd"

defaultMACKey :: String
defaultMACKey = "abcd"

usage :: IO ()
usage = do
    programName <- getProgName
    let header = "USAGE:\n" ++
                 programName ++ " [OPTIONS]\n\n" ++
                 "Calculate hashes etc. of input from stdin\n\n" ++
                 "OPTIONS:"
    hPutStrLn stderr $ usageInfo header options


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
        "Select algorithm [md5,sha1,sha224,sha256,hmac,pbkdf2]",

        Option ['s'] ["salt"] (ReqArg (\arg opt ->
            return opt { saltSource = arg }
        ) "stream")
        ("Input stream for salt to use for key derivation: [default: " ++ defaultSalt ++ "]"),

        Option ['k'] ["key"] (ReqArg (\arg opt ->
            return opt { macKeySource = arg }
        ) "stream")
        ("Input stream for key to use for MAC: [default: " ++ defaultMACKey ++ "]"),

        Option ['i'] ["iterations"] (ReqArg (\arg opt ->
            return opt { iterations = stringToInt arg }
        ) "count")
        ("Iterations to use for key derivation [default: " ++ intToString (iterations defaultOptions) ++ "]"),

        Option ['l'] ["length"] (ReqArg (\arg opt ->
            return opt { derivedKeyLength = stringToInt arg }
        ) "count")
        ("Length of derived key to generate [default: " ++ intToString (derivedKeyLength defaultOptions) ++ " bytes]")
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
        "md5"  -> do
            let digest = runReader (Md5.hash bytes) opts
            putStrLn $ word8ArrayToHexString digest 16

        "sha1" -> do
            let digest = runReader (Sha1.hash bytes) opts
            putStrLn $ word8ArrayToHexString digest 20

        "sha224" -> do
            let digest = runReader (Sha256.hash bytes 28) opts
            putStrLn $ word8ArrayToHexString digest 28

        "sha256" -> do
            let digest = runReader (Sha256.hash bytes 32) opts
            putStrLn $ word8ArrayToHexString digest 32

        "hmac" -> do
            macKeyByteString <- if macKeySource opts == ""
                              then return (BSC.pack defaultMACKey)
                              else BS.readFile (macKeySource opts)
            let macKey = BS.unpack macKeyByteString
            runReaderT (debug'' "[Hmac] key [%d byte(s)]: %s\n" 
                (length macKey)
                (word8ArrayToHexArray macKey (length macKey))) opts

            -- Always use Sha1 as the hash function
            let mac = runReader (Hmac.calculate bytes macKey Sha1.hash 20) opts
            putStrLn $ word8ArrayToHexString mac 32

        "pbkdf2" -> do
            saltByteString <- if saltSource opts == ""
                              then return (BSC.pack defaultSalt)
                              else BS.readFile (saltSource opts)
            let salt = BS.unpack saltByteString
            runReaderT (debug'' "[Pbkdf2] salt [%d byte(s)]: %s\n" 
                (length salt)
                (word8ArrayToHexArray salt (length salt))) opts

            let derivedKey = runReader (Pbkdf2.deriveKey bytes salt 
                                        (iterations opts)
                                        (derivedKeyLength opts)) opts
            putStrLn $ word8ArrayToHexString derivedKey 32

        alg ->
            if alg == ""
            then usage
            else putStrLn $ "Invalid algorithm: " ++ alg

    exitSuccess
