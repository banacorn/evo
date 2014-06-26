module Evo.XCS where

import Evo.Types

import System.Random.MWC

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Debug.Trace
import Data.List (elemIndices)

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
    cond <- liftIO $ replicateM (_chromosomeLength p) (uniform gen)
    action <- liftIO $ uniform gen
    putGen gen
    return (Rule cond action (_initialPrediction p) (_initialError p) (_initialFitness p))

dropNth :: Int -> [a] -> [a]
dropNth i xs = take i xs ++ (tail $ drop i xs)

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
splitActionSet [] = error "spliting empty action set"
splitActionSet matchSet = (chosenAction, maximumPayoff, (chosen, concat (unchosenLeft ++ unchosenRight)))
    where   actionSets = filter (not . null) (groupByAction $ matchSet)
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
-- | GA

deleteRule :: Int -> RuleSet c a -> EvoM (RuleSet c a)
deleteRule n rules = do
    gen <- getGen
    indices <- liftIO $ replicateM n $ uniformR (0, length rules - 1) gen

    let tagged = zip [0..] rules
    let rules' = map snd (foldl (filterPicked indices) [] tagged)

    putGen gen

    return rules'

    where   filterPicked indices acc (index, rule) = if index `elem` indices then acc else (index, rule):acc

pickRule :: Int -> RuleSet c a -> EvoM (RuleSet c a)
pickRule n rules = do
    gen <- getGen
    indices <- liftIO $ replicateM n $ uniformR (0, length rules - 1) gen

    let rules' = foldl (\acc index -> rules !! index : acc) [] indices

    putGen gen

    return rules'

--crossover :: ActionSet c a -> Evo (ActionSet c a)
--crossover rules = do

--    parameter <- ask
--    (papa, mama) <- pickRule 2 rules

--    cut <- liftIO $ uniformR (0, _chromosomeLength parameter - 1) :: EvoM Int

--    let papaCond = _condition papa
--    let mamaCond = _condition mama

--    let offspring0 = Rule cond (_action (_initialPrediction p) (_initialError p) (_initialFitness p)
    --rules' <- deleteRule 2 rules



--------------------------------------------------------------------------------
-- | Full reinforcement process


classify :: (Condition c, Action a) => Classifier c a
classify rules condition = do
            (matched, _) <- coverMatchSet $ splitMatchSet condition rules
            let (action, _, _) = splitActionSet matched
            return action

--reinforce :: (Condition c, Action a) => Classifier c a -> [c] -> (a -> Double) -> Classifier c a
--reinforce parameter rules condition 

coverMatchSet :: (Condition c, Action a) => (MatchSet c a, RuleSet c a) -> EvoM (MatchSet c a, RuleSet c a)
coverMatchSet ((x:xs), unmatched) = return ((x:xs), unmatched)
coverMatchSet ([]    , unmatched) = do

        p <- ask
        gen <- getGen

        -- randomly delete one rule from the rule set
        unmatched' <- deleteRule 1 unmatched

        newAction <- liftIO $ uniform gen

        putGen gen

        -- `#####:Action`
        let newRule = Rule (replicate (_chromosomeLength p) dontCare) newAction (_initialPrediction p) (_initialError p) (_initialFitness p)

        return ([newRule], unmatched')

-- | input ruleset -> (action -> reward) -> updated ruleset
reinforce :: (Condition c, Action a) => RuleSet c a -> [c] -> (a -> Double) -> EvoM (RuleSet c a)
reinforce rules condition evaluate = do

        parameter <- ask

        -- get action
        (matched, unmatched) <- coverMatchSet (splitMatchSet condition rules) -- >>= return . traceShowId

        let (action, predictedPayoff, (chosen, unchosen)) = splitActionSet matched

        -- evaluate action
        let reward = evaluate action

        -- update
        let payoff = actionPayoff parameter predictedPayoff reward
        let updated = updateActionSet parameter payoff chosen


        return $ unmatched ++ unchosen ++ updated


--reinforce :: (Condition c, Action a) =>  RuleSet c a -> [c] -> (a -> Double) -> EvoM (RuleSet c a)
--reinforce rules condition evaluate = do
--    parameter <- ask
--    return $ reinforcePure parameter rules condition evaluate 


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
    population' <- xxx 1000 population
    --liftIO $ print population'

    classify population' [On, On, On, On, On, On, On, On, On, On] >>= liftIO . print

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
    {   _populationSize = 1000
    ,   _learningRate = 0.2
    ,   _discountFactor = 0.7
    ,   _errorBound = 0.01
    ,   _falloffRate = 0.1
    ,   _initialPrediction = 10
    ,   _initialError = 0
    ,   _initialFitness = 10

    ,   _chromosomeLength = 10
    ,   _crossoverProb = 0.8
    ,   _mutationProb = 0.04
    }

go :: IO ()
go = do

    seed <- create >>= save
    runReaderT (evalStateT (runEvoM runner) seed) defaultParameter