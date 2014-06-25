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
    cond <- liftIO $ replicateM 10 (uniform gen)
    action <- liftIO $ uniform gen
    putGen gen
    return (Rule cond action (_initialPrediction p) (_initialError p) (_initialFitness p))

--------------------------------------------------------------------------------
-- | Extract Action Set

-- | splits list into two parts by a predicate
splitList :: (a -> Bool) -> [a] -> ([a], [a])
splitList p = foldl (\(t, f) x -> if p x then (x:t, f) else (t, x:f)) ([], [])

-- | (matched, unmatched)
splitMatchSet :: Condition c => [c] -> RuleSet c a -> (MatchSet c a, RuleSet c a)
splitMatchSet cond = splitList ((== cond) . _condition)

groupByAction :: Action a => MatchSet c a -> [ActionSet c a]
groupByAction rules = map (\a -> filter ((== a) . _action) rules) allActions
    where   allActions = [minBound .. maxBound]

actionPredictedPayoff :: (Condition c, Action a) => ActionSet c a -> Double
actionPredictedPayoff rules = sum (map weightedPayoff rules) / sumOfFitness
    where   sumOfFitness = sum (map _fitness rules)
            weightedPayoff rule = _prediction rule * _fitness rule 

-- | (action, predicted payoff, (chosen, unchosen))
splitActionSet :: (Condition c, Action a) => MatchSet c a -> (a, Double, (ActionSet c a, RuleSet c a))
splitActionSet matchSet = (chosenAction, maximumPayoff, (chosen, concat (unchosenLeft ++ unchosenRight)))
    where   actionSets = groupByAction matchSet
            predictionArray = map actionPredictedPayoff actionSets
            maximumPayoff = maximum predictionArray
            index = head $ elemIndices maximumPayoff predictionArray

            unchosenLeft = take index actionSets
            chosen = actionSets !! index
            unchosenRight = tail $ drop index actionSets

            chosenAction = _action $ head chosen

--------------------------------------------------------------------------------
-- | Update Action Set

actionPayoff :: Parameter -> Double -> Double -> Double
actionPayoff p predictPayoff reward = _discountFactor p * predictPayoff + reward

updateActionSet :: Parameter -> Double -> ActionSet c a -> ActionSet c a
updateActionSet p payoff rules = map updateRule rules
    where   sumOfAccuracy = sum $ map (ruleAccuracy p) rules
            updateRule rule = rule
                {   _prediction = rulePrediction p payoff rule
                ,   _error = ruleError p payoff rule
                ,   _fitness = ruleFitness p sumOfAccuracy rule
                }

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


--------------------------------------------------------------------------------
-- | Full reinforcement process


classify :: (Condition c, Action a) => Classifier c a
classify rules condition = action
    where   (matched, _) = splitMatchSet condition rules
            (action, _, _) = splitActionSet matched

--reinforce :: (Condition c, Action a) => Classifier c a -> [c] -> (a -> Double) -> Classifier c a
--reinforce parameter rules condition 

-- | input ruleset -> (action -> reward) -> updated ruleset
reinforcePure :: (Condition c, Action a) => Parameter -> RuleSet c a -> [c] -> (a -> Double) -> RuleSet c a
reinforcePure parameter rules condition evaluate = unmatched ++ unchosen ++ updated
    where   -- get action
            (matched, unmatched) = traceShowId $ splitMatchSet condition rules
            --matched' = if null matched then 
            (action, predictedPayoff, (chosen, unchosen)) = splitActionSet matched

            -- evaluate action
            reward = evaluate action

            -- update
            payoff = actionPayoff parameter predictedPayoff reward
            updated = updateActionSet parameter payoff chosen


reinforce :: (Condition c, Action a) =>  RuleSet c a -> [c] -> (a -> Double) -> EvoM (RuleSet c a)
reinforce rules condition evaluate = do
    parameter <- ask
    return $ reinforcePure parameter rules condition evaluate 


yyy :: [Bool] -> Bool -> Double
yyy v b = if b == (length (filter id v) > length (filter not v)) then 10 else 0

convert :: Bool -> Bit
convert True = On
convert False = Off

xxx :: Int -> [Vanilla] -> EvoM [Vanilla]
xxx 0 rules = return rules
xxx n rules = do
    gen <- getGen
    boolV <- replicateM 10 (liftIO $ uniform gen) :: EvoM [Bool]
    let bitV = map convert boolV
    rules' <- reinforce rules bitV (yyy boolV)
    putGen gen
    xxx (n-1) rules'

initPopulation :: (Condition c, Action a) => EvoM (RuleSet c a)
initPopulation = do
    p <- ask
    replicateM (_populationSize p) genRule

runner :: EvoM ()
runner = do
    parameter <- ask
    population <- initPopulation :: EvoM [Vanilla]

    liftIO $ print population

    population' <- xxx 100 population

    --let population' = reinforcePure parameter population [On] (\p -> if p then 10 else 0)
    liftIO $ print population'

    liftIO $ print $ classify population' [On]
    liftIO $ print $ classify population' [Off]

    --liftIO $ do
    --    putStrLn "unmatched"
    --    print unmatched

    --    putStrLn "matched yet unchosen"
    --    print unchosen

    --    putStrLn "matched and chosen"
    --    print chosen

    --    putStrLn "chosen action and pridicted payoff"
    --    print (action, predictedPayoff)

    --    putStrLn "calculated payoff"
    --    print payoff

    --    putStrLn "updated action set"
    --    print updated


defaultParameter :: Parameter
defaultParameter = Parameter
    {   _populationSize = 10
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
    runReaderT (evalStateT (runEvoM runner) seed) defaultParameter