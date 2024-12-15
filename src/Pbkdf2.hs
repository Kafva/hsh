module Pbkdf2 (deriveKey, deriveKeyIO) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Data.Bits (xor)
import Types (Config(..), ConfigIO())
import Log (debug')
import Util (word32ToWord8ArrayBE)
import Hmac
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (forM)

-- From the RFC: "In the case of PBKDF2, the "key" is thus the password and the
-- "text" is the salt". I.e.
--  password     --> hmac 'secret key'
--  bytes        --> hmac 'message'
--  With bytes being (salt || INT) in the basecase
prf :: [Word8] -> [Word8] -> Reader Config [Word8]
prf password bytes = do
    Hmac.calculate bytes password

-- Accumulate the hLen blocks of `bytes` into one block with xor
mapAccumXor :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
mapAccumXor accumulator bytes hLen
    | length bytes <= hLen = do
        return (zipWith xor accumulator (take hLen bytes))
    | otherwise = do
        mapAccumXor (zipWith xor accumulator (take hLen bytes)) (drop hLen bytes) hLen

-- Return a flat array of [U_1, U_2, ... U_c]
calculateU :: [Word8] -> [[Word8]] -> Int -> Int -> Reader Config [Word8]
calculateU password accumulator i iterations
    | i == iterations + 1 = return (concat (reverse accumulator))
    | otherwise = do
        -- Pick out the block from the previous iteration from the accumulator
        currentU <- prf password (accumulator!!0)
        -- Note: we prepend the latest U to the accumulator (so that we can use `head`),
        -- the final return value needs to be blockwise reversed with this approach!
        calculateU password (currentU : accumulator) (i+1) iterations

calculateT :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
calculateT password salt blockIndex = do
    cfg <- ask
    let hLen = innerAlgorithmLength cfg
    let s1 = salt ++ word32ToWord8ArrayBE (fromIntegral blockIndex)
    u1 <- prf password s1
    let blocksU = runReader (calculateU password [u1] 2 (iterations cfg)) cfg
    mapAccumXor (replicate hLen 0) blocksU hLen

spawnWorker :: [Word8] -> [Word8] -> Int -> ConfigIO (MVar [Word8])
spawnWorker password salt blockIndex = do
    resultVar <- liftIO newEmptyMVar
    cfg <- ask
    -- Create a thread in the Haskell runtime.
    -- Note: forkOS does not create a different 'type' of thread, the difference
    -- from forkIO lies in ffi compatibility
    _ <- liftIO $ forkIO $ do
        -- putStrLn $ "Processing block #" ++ show blockIndex
        -- threadDelay 1000000
        let r = runReader (calculateT password salt blockIndex) cfg
        putMVar resultVar r
    return resultVar

derivedBlockCount :: Int -> Int -> Int
derivedBlockCount dkLen hLen
    | dkLen > (2 ^ (32 :: Int) - 1) * hLen = error "Derived key length to large"
    | otherwise = if (dkLen `mod` hLen) == 0 then
                            dkLen `div` hLen else
                            -- One extra block if not evenly divisible
                            dkLen `div` hLen + 1

{-
 - PBKDF2 (P, S, c, dkLen)
 -
 - Outer loop:
 -     T_1 = F (P, S, c, 1) ,
 -     T_2 = F (P, S, c, 2) ,
 -     ...
 -     T_l = F (P, S, c, l) ,
 -
 - Inner loop:
 -  F (P, S, c, i) = U_1 \xor U_2 \xor ... \xor U_c
 -
 -     U_1 = PRF (P, S || INT (i)) ,
 -     U_2 = PRF (P, U_1) ,
 -     ...
 -     U_c = PRF (P, U_{c-1})
 -
 - Result:
 -     DK = T_1 || T_2 ||  ...  || T_l<0..r-1>
 -
 - We use HMAC-SHA1 as the PRF.
 -
 - https://www.ietf.org/rfc/rfc2898.txt
 -}
deriveKey :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
deriveKey password salt dkLen = do
    cfg <- ask
    let hLen = innerAlgorithmLength cfg
    let blockCount = derivedBlockCount dkLen hLen

    -- Calculate each block of the derived key.
    -- mapM acts like fmap for functions that return monads (Reader), all
    -- arguments except `i` are fixed.
    ts <- mapM (calculateT password salt) [1..blockCount]

    -- Concatenate all blocks together for the result
    let dk = concat ts
    return dk

deriveKeyIO :: [Word8] -> [Word8] -> Int -> ConfigIO [Word8]
deriveKeyIO password salt dkLen = do
    cfg <- ask
    let hLen = innerAlgorithmLength cfg
    let blockCount = derivedBlockCount dkLen hLen
    debug' "[Pbkdf2] blockCount: %d\n" blockCount

    ts <- if enableThreads cfg then do
        -- The threaded approach is not faster, probably too much overhead
        -- creating and waiting for threads...
        -- Each call launches a new thread and returns a MVar (Mutable variable)
        -- that will be populated with the result for the given block index.
        mVars <- mapM (\i -> do spawnWorker password salt i) [1..blockCount]

        -- Wait for all blocks to be calculated, takeMVar will hang until the
        -- mvar returns a value.
        forM mVars $ \mvar -> liftIO $ takeMVar mvar
    else do
        mapM (\i -> liftIO $ return (runReader (calculateT password salt i) cfg)) [1..blockCount]

    -- Concatenate all blocks together for the result
    let dk = concat ts
    return dk
