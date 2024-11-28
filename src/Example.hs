import Control.Parallel (pseq, par)
--import Control.Concurrent (threadDelay)

timeConsumingTask :: Int -> IO Int
timeConsumingTask _ = do
    --threadDelay (n * 1000 * 1000)  -- Sleep for n seconds
    let x = sum [0..500_000_000]
    putStrLn $ "Task done [" ++ (show x) ++ "]"
    return 7

runPar :: IO ()
runPar = do
    putStrLn "Running in parallel..."
    let a = timeConsumingTask 1
        b = timeConsumingTask 1
        c = timeConsumingTask 1
    r <- a `par` b `par` c `pseq` sequence [a,b,c]
    print $ show r

runSeq :: IO ()
runSeq = do
    putStrLn "Running \"normally\"..."
    a <- timeConsumingTask 1
    b <- timeConsumingTask 1
    c <- timeConsumingTask 1
    let r = [a,b,c]
    print $ show r

main :: IO ()
main = do
    --runSeq
    runPar
