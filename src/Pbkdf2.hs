module Pbkdf2 (deriveKey) where

import Data.Binary (Word8)
import Control.Monad.Reader
import Data.Bits (xor)
import Types (Config(..), ConfigMonad())
import Log (debug')
import Util (word32ToWord8ArrayBE, word8ArrayToHexArray)
import Hmac
import Control.Concurrent.MVar
import Control.Concurrent

-- From the RFC: "In the case of PBKDF2, the "key" is thus the password and the
-- "text" is the salt". I.e.
--  password     --> hmac 'secret key'
--  bytes        --> hmac 'message'
--  With bytes being (salt || INT) in the basecase
prf :: [Word8] -> [Word8] -> Reader Config [Word8]
prf password bytes = do
    Hmac.calculate bytes password

-- Accumlate the hLen blocks of `bytes` into one block with xor
mapAccumXor :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
mapAccumXor accumlator bytes hLen
    | length bytes <= hLen = do
        return (zipWith xor accumlator (take hLen bytes))
    | otherwise = do
        mapAccumXor (zipWith xor accumlator (take hLen bytes)) (drop hLen bytes) hLen

-- Return a flat array of [U_1, U_2, ... U_c]
calculateU :: [Word8] -> [[Word8]] -> Int -> Int -> Reader Config [Word8]
calculateU password accumlator i iterations
    | i == iterations + 1 = return (concat (reverse accumlator))
    | otherwise = do
        -- Pick out the block from the previous iteration from the accumlator
        currentU <- prf password (head accumlator)
        -- Note: we prepend the lastest U to the accumlator (so that we can use `head`),
        -- the final return value needs to be blockwise reversed with this approach!
        calculateU password (currentU : accumlator) (i+1) iterations

calculateT :: [Word8] -> [Word8] -> Int -> Reader Config [Word8]
calculateT password salt blockIndex = do
    cfg <- ask
    let hLen = innerAlgorithmLength cfg
    let s1 = salt ++ word32ToWord8ArrayBE (fromIntegral blockIndex)
    u1 <- prf password s1
    let blocksU = runReader (calculateU password [u1] 2 (iterations cfg)) cfg
    mapAccumXor (replicate hLen 0) blocksU hLen


spawnWorker :: [Word8] -> [Word8] -> Int -> ConfigMonad (MVar [Word8])
spawnWorker password salt blockIndex = do
    resultVar <- liftIO newEmptyMVar
    cfg <- ask
    _ <- liftIO $ forkIO $ do
        putStrLn $ "Starting work... [" ++ show blockIndex ++ "]"
        threadDelay 1000000
        let result = runReader (calculateT password salt blockIndex) cfg
        putMVar resultVar result
    return resultVar

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
 -     U_c = PRF (P, U_{c-1}) .
 -
 - Result:
 -     DK = T_1 || T_2 ||  ...  || T_l<0..r-1>
 -
 - We use HMAC-SHA1 as the PRF.
 -
 - https://www.ietf.org/rfc/rfc2898.txt
 -}
deriveKey :: [Word8] -> [Word8] -> ConfigMonad [Word8]
deriveKey password salt = do
    cfg <- ask
    let dkLen = derivedKeyLength cfg
    let hLen = innerAlgorithmLength cfg

    when (dkLen > (2 ^ (32 :: Int) - 1) * hLen) $ error "Derived key length to large"

    let lastBlockByteCount = dkLen `mod` hLen
    let derivedBlockCount = if lastBlockByteCount == 0 then
                                dkLen `div` hLen else
                                -- One extra block if not evenly divisible
                                dkLen `div` hLen + 1

    let blocksPerWorker = derivedBlockCount `div` jobs cfg

    -- The default step is 2
    let blockIndexes = takeWhile (<= derivedBlockCount) [1, blocksPerWorker + 1 ..]

    debug' "[Pbkdf2] blocksPerWorker: %d\n" blocksPerWorker
    debug' "[Pbkdf2] derivedBlockCount: %d\n" derivedBlockCount

    ts <- forM blockIndexes $ \blockIndex -> do
        let lastBlockIndex = if blockIndex + blocksPerWorker - 1 <= derivedBlockCount then
                                blockIndex + blocksPerWorker - 1
                            else derivedBlockCount

        -- mapM acts like fmap for functions that return monads (Reader), all
        -- arguments except `i` are fixed.
        -- Each call launches a new thread and returns a MVar (Mutable variable)
        -- that will be populated with the result for the given block index.
        mVars <- mapM (\i -> do spawnWorker password salt i) [blockIndex..lastBlockIndex]

        -- Wait for all blocks to be calculated, takeMVar will hang until the 
        -- mvar returns a value.
        forM mVars $ \mvar -> liftIO $ takeMVar mvar
    
    -- Concatenate all blocks together for the result
    let dk = concat $ concat ts

    return dk
