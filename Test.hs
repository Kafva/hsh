-- https://hoogle.haskell.org/?hoogle=format
-- https://www.haskell.org/tutorial/intro.html
-- http://learnyouahaskell.com/introduction
-- ghc Main.hs -o sha && ./sha
--
-- =============================================================================
-- Monads ("computation builders")
-- A monad is essentially a pattern for chaining operations,
-- Any language that supports higher order functions (functions that
-- accept functions as arguments to other functions) can use
-- a monad pattern.
--
-- A monad consists of:
--  1. A Monadic type: 'Maybe' / Optional<>
--  2. A unit operation to convert from the Monadic type to the wrapped type: 
--    Just()  / Some()
--  3. A bind opertation for chaining monadic types: '>>='
--  
-- 1-3 can be defined in several ways, 'Maybe' is just an example.
-- for the 'identity monad' '>>=' is essentially just a ';'
--
-- Functions in the monad pattern need to accept and return the same type.
--  (node)  [1,2,3].flatMap(a=>[a+1]).flatMap(b => b==2 ? [b] : [b+1]  )
-- We use flatMap() so that each function returns the original type (a list)
--
-- The '>>=' (bind) operator works like flatMap() (for lists) 
-- and thus chains input between functions
--
--  The 'do' operator is syntactic sugar for putting '>>=' on every line
--  '\' in Haskell is essentially the same as 'lambda' in py, i.e. it is used 
--  to define anonymous functions.
--
--  Functions that can return 'Nothing' would need a 'if Nothing' check for each '>>='
--  In a 'do' block that uses the 'Failure Monad', execution will implicitly 
--  pass on 'Nothing' through the entire chain once _one_ statement returns 'Nothing'
--
--
-- If statements must have an else block.
-- Variables in Haskell cannot be captialised
--
-- View definition of function (from ghci)
-- :info printf
--
-- OPERATORS
--  The difference (kind of) between '=' and '<-'
--    let x = func      // let a = func
--    let x = func()    // a <- func
--
-- Dollar sign is effectively parenthesis
--  sort ("b" ++ "a") 
--
--  sort $ "b" ++ "a"
-- '.' is basically the function composition operator
--
-- f(g(x)) <===> f.g(x)
--
-- <> is an alias for 'mappend' which concatenates two lists.
-- 
-- List indexing is done using '!!'
--  [1,2,3]!!0
--  => 1
-- =============================================================================
module Main where

import Data.List
import System.IO
import System.Directory
import System.Environment -- getArgs
import Text.Printf
import Data.Foldable

import Debug.Trace

-- `print` is for debugging, use `putStrLn` for actual output.

-- Functions can be given several definitions in Haskell through
-- pattern matching of arguments, e.g. if second argument == 0.
-- The '|' conditional guard is used if the condition for the body
-- is not a single value.
--
-- Note: the function operator '->' takes precedence from the right 
--    Integer -> Bool -> Integer 
--    Integer -> (Bool -> Integer)
--
-- The last type is the return type and everything else is arguments
-- In practice, a Haskell function only ever takes on argument, with each
-- argument being bound to a type in the signature. Translating a function
-- with several parameters into a chain of functions with one argument is called 'currying'
-- E.g. a function that accepts the tuple (a,b) and returns c would be curried as:
--
--  f :: a -> (b -> c) 
-- 
-- Each (a -> b) function is then lazily evaluated in order of precedence.
-- 
-- Type signatures that use '=>' indicate that an argument must be of a specific type
-- e.g. Num a => Integer
boo :: Integer -> Bool -> Integer
boo x y | (mod x 2 == 1) = trace("It's not divisable")  $ 1
        | (mod x 2 == 0) = trace("It's divisable") $ 2

cool :: String -> Bool
cool s = s == "wow"

-- Impure functions are cleanly separated in Haskell from pure functions
-- putStrLn() returns an IO action
-- IO is the keyword that denotes impure functions
-- Unless we use 'do' we can only have one print statement, to use 'printf-debugging'
-- we should rely on 'trace' (which does not haft to be within in the main monad)

printer :: PrintfType r => String -> r
printer s = printf "-> %s" s

index :: Int
index = 0

-- =============================================================================
-- https://wiki.haskell.org/All_About_Monads#Why_should_I_make_the_effort_to_understand_monads.3F

-- (This is a mirrored implementation of `Maybe`)
--
-- `Optional` is a _type constructor_ since we give the arbritrary label 'a'
-- `Some` and `Nothing` are _data constructors_, i.e. they wrap data.
data Optional a = None | Some a

-- With this we can define functions like:
myColor :: String -> Optional String
myColor s = do
  if (s == "") then None
               else Some "That's not empty"

-- To "unwrap" a maybe value
unwrap :: Optional a -> a
unwrap None = error("PANIC in unwrap()")
unwrap (Some a) = a

-- Monads are essentially _type constructors_, i.e. they wrap values
-- We have the monad itself (Optional or 'm')
--
--  (return) A function that wraps:      a -> Optional a
--  (>>=) Takes an arbitary optional of some type ('a') and a computation that
--  combines the unwrapped 'a' with an Optional of (potentially) different type 'b'.
--
--    Optional a -> (a -> Optional b) -> Optional b 
--
-- See how a 'do' block basically has a (>>=) (also called bind) on every line.
-- Bind accepts a wrapped value, and does some computation to produce a new wrapped value

-- Custom Mondas should preferably be defined using the Monad baseclass
class Rmonad m where
  bind   :: m a -> (a -> m b) -> m b 
  return :: a -> m a


main :: IO ()
main = do
  args <- getArgs
  printf "ARGV[%d]: %20s\n" index $ if length args > index 
    then args!!index
    else printf "Less than %d argument(s)\n" index

  if cool "xd" 
  then print "yep"
  else print "no"


  let x = map printer args :: [String]
  putStr $ unlines x

  -- Folding, kind of like reduce()
  print $ foldl (+) 0 [1,2,3,4] -- => 10


  let q = [1,2,3] >>= \a -> [a+1] >>= \b -> if b==2 then [b] else [boo b True]

  
  let color = Some "red"
  putStrLn $ unwrap $ myColor "haha"




  print q

