module Evo where

import Evo.Type
import Evo.GA

import Control.Monad
import Control.Monad.Reader
import Data.STRef
import System.Random.MWC
-- import Data.Vector (singleton)

main :: IO ()
main = do
    seed <- createSystemRandom >>= save
    print $ runEvo seed $ do
        let len = 100
        let size = 1000
        popRef <- populate len size >>= lift . newSTRef
        replicateM 20 $ round popRef
        -- return $ sum $ map fitness population'''''

    where
        round popRef = do
            population <- lift $ readSTRef popRef

            pairs <- replicateM (length population `div` 2) (select population)
            offspring <- mapM (crossover 1) pairs
            offspring' <- mapM (mutate 0.001) (unpair offspring)

            lift $ writeSTRef popRef offspring'
            return $ sum $ map fitness offspring'
