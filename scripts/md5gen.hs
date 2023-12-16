#!/usr/bin/env runhaskell
{-
  Generate the lookup table used for MD5 digest calculation.

   for i from 0 to 63 do
       K[i] := floor(2**32 * abs (sin(i + 1)))
   end for
-}
main :: IO ()
main = do
  let table = map (\i -> floor $ (2**32 * (abs $ sin $ i+1)))  [0..63]
  putStrLn $ show table
