module Evo where

import Evo.Types

import System.Random.MWC

import Control.Monad

main = do



    gen <- create

    v <- replicateM 100 (uniformR (Off, DontCare) gen) :: IO [Bit]

    print v