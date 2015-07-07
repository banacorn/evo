{-# LANGUAGE RankNTypes #-}

module Main where

import Evo.Type
import Evo.GA

import Control.Monad
import System.Random.MWC
import Data.Vector (singleton)

seed :: Seed
seed = toSeed (singleton 42)

main :: IO ()
main = print $ runEvo seed $ do

    p0 <- populate 5 1
    p1 <- populate 5 1

    return (p0, p1)
