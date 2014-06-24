module Evo where

import Evo.Types

import System.Random.MWC

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Debug.Trace
import Data.List (elemIndices)
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
    p <- ask
    gen <- getGen
    cond <- liftIO $ uniform gen
    action <- liftIO $ uniform gen
    putGen gen
    return (Rule [cond] action (_initialPrediction p) (_initialError p) (_initialFitness p))


match :: Condition c => [c] -> [Rule c a] -> MatchSet c a
match cond = filter ((== cond) . _condition)

groupByAction :: Action a => MatchSet c a -> [ActionSet c a]
groupByAction rules = map (\a -> filter ((== a) . _action) rules) allActions
    where   allActions = [minBound .. maxBound]

actionPayoff :: Action a => ActionSet c a -> Double
actionPayoff rules = sum (map weightedPayoff rules) / sumOfFitness
    where   sumOfFitness = sum (map _fitness rules)
            weightedPayoff rule = _prediction rule * _fitness rule 

chooseAction :: Action a => MatchSet c a -> ActionSet c a
chooseAction matchSet = actionSets !! index
    where   actionSets = groupByAction matchSet
            predictionArray = map actionPayoff actionSets
            index = head $ elemIndices (maximum predictionArray) predictionArray

--------------------------------------------------------------------------------
-- | Update Action Set
{-
    [P] - match -> [M] - predictedPayoffBasedOnAction -> [A]

-}

--updateActionSet :: Parameter -> Double -> ActionSet c a -> ActionSet c ca
--updateActionSet p reward rules = map updateRule rules
--    where   payoff = calculatePayoff p reward rules
--            sumOfAccuracy = sumAccuracy p rules

--            updateRule rule = rule
--                {   _prediction = 

--            }


--updateActionSet :: Parameter -> Double -> ActionSet c a -> ActionSet c ca
--updateActionSet p reward rules = map updateRule rules
--    where   payoff = calculatePayoff p reward rules
--            sumOfAccuracy = sumAccuracy p rules

--            updateRule rule = rule
--                {   _prediction = 

--            }

rulePayoff :: Action a => Parameter -> Double -> ActionSet c a -> Double
rulePayoff p reward rules = _discountFactor p * maximum (map predictPayoff (groupByAction rules)) + reward
    where   predictPayoff rules' = sum (map weightedPayoff rules') / sumOfFitness
            sumOfFitness = sum (map _fitness rules)
            weightedPayoff rule = _prediction rule * _fitness rule 

ruleAccuracy :: Parameter -> Rule c a -> Double
ruleAccuracy p rule = 1 `max` exp (log a * ((e - e0) / e0))
    where   a = _falloffRate p
            e0 = _errorBound p
            e = _error rule

ruleFitness :: Parameter -> Double -> Rule c a -> Double
ruleFitness p sumOfAccuracy rule = f + _learningRate p * (k' - f)
    where   f = _fitness rule
            k' = ruleAccuracy p rule / sumOfAccuracy

ruleError :: Parameter -> Double -> Rule c a -> Double
ruleError p payoff rule = e + b * (abs (payoff - _prediction rule) - e)
    where   e = _error rule
            b = _learningRate p

rulePrediction :: Parameter -> Double -> Rule c a -> Double
rulePrediction p payoff rule = pr + b * (payoff - pr) 
    where   pr = _prediction rule
            b = _learningRate p











initPopulation :: (Condition c, Action a) => Int -> EvoM [Rule c a]
initPopulation n = replicateM n genRule

runner :: EvoM ()
runner = do
    p <- ask
    population <- initPopulation 100 :: EvoM [Vanilla]
    liftIO . print . chooseAction . match [On] $ population


parameter :: Parameter
parameter = Parameter
    {   _populationSize = 1000 
    ,   _learningRate = 0.2
    ,   _discountFactor = 0.7
    ,   _errorBound = 0.01
    ,   _falloffRate = 0.1
    ,   _initialPrediction = 10
    ,   _initialError = 0
    ,   _initialFitness = 10
    }

main :: IO ()
main = do

    seed <- create >>= save
    runReaderT (evalStateT (runEvoM runner) seed) parameter