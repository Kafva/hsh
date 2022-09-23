-- https://hoogle.haskell.org/?hoogle=format
import Text.Printf
import System.Exit
import Control.Monad -- `when`
import Data.List
import System.Environment -- getArgs
import System.IO
import System.Console.GetOpt
import Debug.Trace
import qualified Data.ByteString.Lazy as BL

import qualified Md5 as Md5

programVersion = "0.1.0"
data Algorithm = MD5 | SHA1 | SHA256

-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
-- Dictionary of CLI options
-- Note the use of 'deriving Show' to allow for the data to be
-- easily shown to stdout.
--
data Flags = Flags { 
  help :: Bool,
  version :: Bool,          
  debug :: Bool,
  algorithm :: Integer
} deriving Show

defaultOptions :: Flags
defaultOptions =  Flags {
  help = False,
  version = False,
  debug = False,
  algorithm = 1
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
options = 
  [ 
    Option ['V'] ["version"] (NoArg (\_ -> do 
      putStrLn programVersion 
      exitWith ExitSuccess
    )) "Show version",
    Option ['h'] ["help"] (NoArg (\_ -> do 
      prg <- getProgName
      hPutStrLn stderr $ usageInfo ("usage: "++prg) options
      exitWith ExitSuccess
    )) "Print help information",
    Option ['d'] ["debug"] (NoArg (\opt ->
      return opt { debug = True }
    )) "Print debug output",
    Option ['a'] ["algorithm"] (ReqArg (\arg opt ->
      -- Note that `ReqArg` has two arguments for its function
      -- `read` will perform string->int conversion
      return opt { algorithm = read arg }
    ) "ALG")
    "Select algorithm (1-3)"
  ]

main :: IO ()
main = do
  args <- getArgs
  -- `actions` will hold the (Flags -> IO Flags) functions defined for each flag
  let (actions, nonOptions, errors) = getOpt RequireOrder options args 

  -- With `foldl`, we apply the bind operator to each function in `actions`
  --  Recall that `return` wraps a value in a monad
  opts <- foldl (>>=) (return defaultOptions) actions 

  -- Access to fields: 'object.field' -> 'field object'
  when (debug opts) $ putStrLn $ show opts

  -- Reads from stdin (does not wait when no input is given)
  input <- BL.getContents

  let out = Md5.hash input

  when (debug opts) $ do
    putStrLn $ show out
    putStrLn $ show $ length out

