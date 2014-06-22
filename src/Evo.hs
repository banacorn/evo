module Evo where

import Evo.Types

import System.Random.MWC

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

-- LCS
-- 1. find the matched set [M]
-- 2. group the matched set by actions [A]
-- 3. 

getGen :: EvoM GenIO
getGen = get >>= liftIO . restore

putGen :: GenIO -> EvoM ()
putGen gen = liftIO (save gen) >>= put

--uniform' :: Variate a => EvoM a
--uniform' = do
--    gen <- getGen
--    n <- liftIO $ uniform gen


genRule :: (Condition c, Action a) => EvoM (Rule c a)
genRule = do
    gen <- getGen
    cond <- liftIO $ uniform gen
    action <- liftIO $ uniform gen
    putGen gen
    return (Rule [cond] action 100 0 100)


match :: Condition c => [c] -> [Rule c a] -> [Rule c a]
match cond = filter ((== cond) . condition)

groupByAction :: Action a => [Rule c a] -> [[Rule c a]]
groupByAction rules = map (\a -> filter ((== a) . action) rules) allActions
    where   allActions = [minBound .. maxBound]

predictPayoff :: [Rule c a] -> Double
predictPayoff rules = sum (map weightedPayoff rules) / sumOfFitness
    where   sumOfFitness = sum (map fitness rules)
            weightedPayoff rule = payoff rule * fitness rule

initPopulation :: (Condition c, Action a) => Int -> EvoM [Rule c a]
initPopulation n = replicateM n genRule

runner :: EvoM ()
runner = do
    p <- initPopulation 100 :: EvoM [Vanilla]
    liftIO $ print . predictPayoff . head . groupByAction $ match [On] p


parameter :: Parameter
parameter = Parameter
    {   populationSize = 1000 
    ,   learningRate = 0.2
    ,   discountFactor = 0.7
    ,   errorBound = 0.01
    ,   falloffRate = 0.1
    ,   initialPrediction = 10
    ,   initialError = 0
    ,   initialFitness = 10
    }

main :: IO ()
main = do
    seed <- create >>= save
    runReaderT (evalStateT (runEvoM runner) seed) parameter