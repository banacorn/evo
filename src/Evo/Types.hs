{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Evo.Types where

import System.Random.MWC
import Control.Monad.Primitive

data Rule cond action reward = Rule [cond] action reward

data Bit = On | Off | DontCare deriving Enum

--instance PrimMonad m => Functor m where
--    fmap f action = do
--        result <- action
--        result (f result)

instance Show Bit where
    show On = "0"
    show Off = "1"
    show DontCare = "#"

instance Variate Bit where
    uniform gen = uniform gen >>= return . toEnum . flip mod 3
    uniformR (a, b) gen = uniformR (fromEnum a, fromEnum b) gen >>= return . toEnum

type Vanilla = Rule Bit Bool Int