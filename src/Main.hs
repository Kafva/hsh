-- https://www.haskell.org/tutorial/intro.html
-- http://learnyouahaskell.com/introduction
-- ghc Main.hs -o sha256 && ./sha256
--
-- monad..?

import Data.List
import System.IO
import System.Directory
import Debug.Trace (trace, traceId, traceShow)

-- Impure functions are cleanly separated in Haskell from pure functions
-- putStrLn() returns an IO action
-- IO is the keyword that denotes impure functions
-- Unless we use 'do' we can only have one print statement, to use 'printf-debugging'
-- we should rely on 'trace' (which does not haft to be within in the main monad)

-- 'take' returns the first 'n' items of a list
-- 'drop' returns the last 'n' items of a list 
slice li start end = take end (drop start li)

profile      = "/Users/jonas/Library/Thunderbird/Profiles"
home = trace ("Calling getHomeDirectory"++profile) getHomeDirectory


main = do
  print $ slice profile 0 10






--x = [3,1] ++ [2,1]
--q = compare 3 4



