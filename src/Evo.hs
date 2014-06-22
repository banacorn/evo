module Evo where

import Evo.Types

import System.Random.MWC

import Control.Monad
import Control.Monad.State


-- LCS
-- 1. find the matched set [M]
-- 2. group the matched set by actions [A]
-- 3. 

--initPopulation :: IO [Rule]
--initPopulation = do

getGen :: EvoM GenIO
getGen = get >>= liftIO . restore

putGen :: GenIO -> EvoM ()
putGen gen = liftIO (save gen) >>= put

--uniform' :: Variate a => EvoM a
--uniform' = do
--    gen <- getGen
--    n <- liftIO $ uniform gen


genRule :: EvoM Vanilla
genRule = do
    gen <- getGen
    cond <- liftIO $ uniform gen
    action <- liftIO $ uniform gen
    putGen gen
    return $ Rule [cond] action 100

initPopulation :: Int -> EvoM [Vanilla]
initPopulation n = replicateM n genRule

main = do
    seed <- create >>= save
    flip evalStateT seed $ runEvoM $ do
        p <- initPopulation 100
        liftIO $ print p
