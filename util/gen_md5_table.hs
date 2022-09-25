{-
  Generate the lookup table used for MD5 digest calculation.

   for i from 0 to 63 do
       K[i] := floor(2**32 * abs (sin(i + 1)))
   end for
-}

fn :: Float -> Int
fn i = floor (2**32 * (abs $ sin (i + 1)))

main :: IO ()
main = do
  let table = map fn [0..63]
  --putStrLn $ show table
  --putStrLn $ show (table!!0)
  putStrLn $ show $ floor $ (2**32 * (abs $ sin 1))

