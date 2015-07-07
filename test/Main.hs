{-# LANGUAGE RankNTypes #-}

module Main where

import Evo.Type
import Evo.GA

import Control.Monad

main :: IO ()
main = print $ runEvo seed $ do

    population <- populate 5 10

    


    return ()
