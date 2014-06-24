{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Evo.Types where

import System.Random.MWC
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Base
import Control.Applicative (Applicative)
import Control.Monad.Trans.Control

newtype EvoM a = EvoM { runEvoM :: StateT Seed (ReaderT Parameter IO) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadState Seed, MonadReader Parameter, MonadBase IO)

instance (MonadBaseControl IO) EvoM where
    newtype StM EvoM a = StMEvo { unStMCEvo :: StM (StateT Seed (ReaderT Parameter IO)) a }
    liftBaseWith f = EvoM (liftBaseWith (\run -> f (liftM StMEvo . run . runEvoM)))
    restoreM = EvoM . restoreM . unStMCEvo

data Parameter = Parameter
    {   _populationSize :: Int 
    ,   _learningRate :: Double
    ,   _discountFactor :: Double
    ,   _errorBound :: Double
    ,   _falloffRate :: Double
    ,   _initialPrediction :: Double
    ,   _initialError :: Double
    ,   _initialFitness :: Double
    }

type MatchSet c a= [Rule c a]
type ActionSet c a= [Rule c a]

data Rule c a = Rule
    {   _condition :: [c]
    ,   _action :: a
    ,   _prediction :: Double
    ,   _error :: Double
    ,   _fitness :: Double
    }

instance (Show c, Show a) => Show (Rule c a) where
    show (Rule cond action prediction err fitness) = "< " ++ concat (map show cond) 
        ++ " : " ++ show action 
        ++ " = " ++ show prediction 
        ++ " " ++ show err
        ++ " " ++ show fitness
        ++ " >"

class (Eq c, Enum c, Variate c) => Condition c where
class (Eq a, Enum a, Variate a ,Bounded a) => Action a where

data Bit = On | Off | DontCare deriving Enum

instance Eq Bit where
    On == Off = False
    On == _ = True
    Off == On = False
    Off == _ = True
    DontCare == _ = True

instance Show Bit where
    show On = "0"
    show Off = "1"
    show DontCare = "#"

instance Condition Bit
instance Action Bool

instance Variate Bit where
    uniform gen = uniform gen >>= return . toEnum . flip mod 3
    uniformR (a, b) gen = uniformR (fromEnum a, fromEnum b) gen >>= return . toEnum

type Vanilla = Rule Bit Bool