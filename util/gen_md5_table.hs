{-
  Generate the lookup table used for MD5 digest calculation.

   for i from 0 to 63 do
       K[i] := floor(2**32 * abs (sin(i + 1)))
   end for
-}



--table = [] 



fn :: Float -> Int
fn i = floor $ fromIntegral 2**32 * (abs $ sin (i+1))
--  where
--  i = fromIntegral i :: Float

main :: IO ()
main = do
  putStrLn $ show $ fn 6

