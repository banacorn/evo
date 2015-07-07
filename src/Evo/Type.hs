{-# LANGUAGE RankNTypes #-}

module Evo.Type where

import System.Random.MWC
import Control.Monad.ST
import Control.Monad.State

type Genotype = [Bool]
type Phenotype = Genotype
type Evo s a = StateT Seed (ST s) a

getGen :: Evo s (GenST s)
getGen = get >>= lift . restore

putGen :: GenST s -> Evo s ()
putGen gen = lift (save gen) >>= put

runEvo :: Seed -> (forall s. Evo s a) -> a
runEvo seed f = runST $ evalStateT f seed


-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Base
-- import Control.Applicative (Applicative)
-- import Control.Monad.Trans.Control

-- newtype EvoM a = EvoM { runEvoM :: StateT Seed (ReaderT Parameter IO) a }
--     deriving (Monad, Functor, Applicative, MonadIO, MonadState Seed, MonadReader Parameter, MonadBase IO)

-- instance (MonadBaseControl IO) EvoM where
--     newtype StM EvoM a = StMEvo { unStMCEvo :: StM (StateT Seed (ReaderT Parameter IO)) a }
--     liftBaseWith f = EvoM (liftBaseWith (\run -> f (liftM StMEvo . run . runEvoM)))
--     restoreM = EvoM . restoreM . unStMCEvo

--data Parameter = Parameter
--    {   _populationSize :: Int
--    ,   _learningRate :: Double
--    ,   _discountFactor :: Double
--    ,   _errorBound :: Double
--    ,   _falloffRate :: Double
--    ,   _initialPrediction :: Double
--    ,   _initialError :: Double
--    ,   _initialFitness :: Double

--    ,   _chromosomeLength :: Int
--    ,   _crossoverProb :: Double
--    ,   _mutationProb :: Double
--    }

--type RuleSet c a = [Rule c a]
--type MatchSet c a = RuleSet c a
--type ActionSet c a = RuleSet c a
--type Classifier c a = RuleSet c a -> [c] -> EvoM a
--type Model c a = [c] -> a

--data Rule c a = Rule
--    {   _condition :: [c]
--    ,   _action :: a
--    ,   _prediction :: Double
--    ,   _error :: Double
--    ,   _fitness :: Double
--    }

--instance (Show c, Show a) => Show (Rule c a) where
--    show (Rule cond action prediction err fitness) = "< " ++ concat (map show cond)
--        ++ " : " ++ show action
--        ++ " = " ++ show prediction
--        ++ " " ++ show err
--        ++ " " ++ show fitness
--        ++ " >"

--class (Eq c, Enum c, Show c, Variate c, Bounded c) => Condition c where
--    dontCare :: c

--class (Eq a, Enum a, Show a, Variate a, Bounded a) => Action a where

--data Bit = DontCare | On | Off deriving (Enum, Bounded)

--instance Eq Bit where
--    On == Off = False
--    On == _ = True
--    Off == On = False
--    Off == _ = True
--    DontCare == _ = True

--instance Show Bit where
--    show On = "1"
--    show Off = "0"
--    show DontCare = "#"

--instance Condition Bit where
--    dontCare = DontCare
--instance Action Bool

--instance Variate Bit where
--    uniform gen = uniform gen >>= return . toEnum . flip mod 3
--    uniformR (a, b) gen = uniformR (fromEnum a, fromEnum b) gen >>= return . toEnum

--type Vanilla = Rule Bit Bool