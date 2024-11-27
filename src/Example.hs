import Control.Parallel (par, pseq)
import Control.Concurrent (threadDelay)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.Monad (forM)

timeConsumingTask :: Int -> IO Int
timeConsumingTask n = do
    threadDelay (n * 1000 * 1000)  -- Sleep for n seconds
    putStrLn $ "Task done [" ++ show n ++ "]"
    return 7

runSeq :: IO ()
runSeq = do
     start <- getCPUTime
     mapM_ timeConsumingTask [1, 1, 1]
     end <- getCPUTime
     let diff = fromIntegral (end - start) / (10^12)
     printf "Sequential execution time: %0.3f sec\n" (diff :: Double)

runPar :: IO ()
runPar = do
    start <- getCPUTime


    printf "Starting...\n"
    results <- forM [1..4] $ \i -> do
        let result = timeConsumingTask 5
        result `par` return result
    
    -- Wait for parallel execution to finish, `seq` makes sure that both of its 
    -- arguments are evaluated before returning.
    printf "Waiting...\n"
    mapM_ (\r -> r `seq` return ()) results
    
    -- Unwrap from [Reader Config [Word8]] -> [[Word8]]
    ts <- sequence results

    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Parallel execution time: %0.3f sec\n" (diff :: Double)

main :: IO ()
main = do
    runSeq
    runPar
