module Evo where

import Evo.Types

import System.Random.MWC

import Control.Monad
import Control.Monad.State

--initPopulation :: IO [Rule]
--initPopulation = do



genRule :: EvoM Vanilla
genRule = do

    gen <- get >>= liftIO . restore

    cond <- replicateM 10 (liftIO $ uniform gen)
    action <- liftIO $ uniform gen
    liftIO (save gen) >>= put

    return $ Rule cond action 100


--printSeed gen = save gen >>= print

main = do
    seed <- create >>= save

    flip evalStateT seed $ runEvoM $ do
        genRule >>= liftIO . print
        genRule >>= liftIO . print
        genRule >>= liftIO . print
        genRule >>= liftIO . print
