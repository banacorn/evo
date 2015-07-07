{-# LANGUAGE RankNTypes #-}

module Evo.Type where

import System.Random.MWC
import Data.STRef
import Control.Monad.ST
import Control.Monad.Reader

type Genotype = [Bool]
type Phenotype = Genotype
type Population = [Genotype]
data Ref s = Ref
    {   gen :: STRef s (GenST s)
    }
type Evo s a = ReaderT (Ref s) (ST s) a

getGen :: Evo s (GenST s)
getGen = asks gen >>= lift . readSTRef

putGen :: GenST s -> Evo s ()
putGen g = do
    ref <- asks gen
    lift $ writeSTRef ref g

rand :: Variate a => Evo s a
rand = getGen >>= lift . uniform

randR :: Variate a => (a, a) -> Evo s a
randR (a, b) = getGen >>= lift . uniformR (a, b)

runEvo :: Seed -> (forall s. Evo s a) -> a
runEvo seed f = runST $ do
    genRef <- restore seed >>= newSTRef
    runReaderT f (Ref genRef)


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
