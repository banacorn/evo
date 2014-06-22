{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Evo.Types where

import System.Random.MWC
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Base
import Control.Monad.Primitive (PrimState)
import Control.Applicative (Applicative)
import Control.Monad.Trans.Control

newtype EvoM a = EvoM { runEvoM :: StateT Seed (ReaderT Parameter IO) a }
    deriving (Monad, Functor, Applicative, MonadIO, MonadState Seed, MonadBase IO)

instance (MonadBaseControl IO) EvoM where
    newtype StM EvoM a = StMEvo { unStMCEvo :: StM (StateT Seed (ReaderT Parameter IO)) a }
    liftBaseWith f = EvoM (liftBaseWith (\run -> f (liftM StMEvo . run . runEvoM)))
    restoreM = EvoM . restoreM . unStMCEvo

data Parameter = Parameter
    {   populationSize :: Int 
    ,   learningRate :: Double
    ,   discountFactor :: Double
    ,   errorBound :: Double
    ,   falloffRate :: Double
    ,   initialPrediction :: Double
    ,   initialError :: Double
    ,   initialFitness :: Double
    }

data Rule c a = Rule
    {   condition :: [c]
    ,   action :: a
    ,   payoff :: Double
    ,   error :: Double
    ,   fitness :: Double
    }

instance (Show c, Show a) => Show (Rule c a) where
    show (Rule cond action payoff error fitness) = "< " ++ concat (map show cond) 
        ++ " : " ++ show action 
        ++ " = " ++ show payoff 
        ++ " " ++ show error
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