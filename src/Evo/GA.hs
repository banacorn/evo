module Evo.GA where

import Evo.Type

import System.Random.MWC
-- import Control.Monad.ST
import Control.Monad.State
import Data.Vector (singleton)

seed :: Seed
seed = toSeed (singleton 42)

decode :: Genotype -> Phenotype
decode = id

fitness :: Genotype -> Int
fitness = sum . map f
    where   f True = 1
            f False = 0

individual :: Genotype
individual = replicate 50 False

population :: [Genotype]
population = replicate 100 individual

crossoverAt :: Int -> (Genotype, Genotype) -> (Genotype, Genotype)
crossoverAt n (a, b) = (a', b')
    where   a' = take n a ++ drop n b
            b' = take n b ++ drop n a

mutateAt :: Int -> Genotype -> Genotype
mutateAt n gene = take n gene ++ [not (gene !! n)] ++ drop (n + 1) gene


rand :: Variate a => Evo s a
rand = do
    gen <- getGen
    num <- lift (uniform gen)
    putGen gen
    return num

populate :: Int -> Int -> Evo s [Genotype]
populate len n = do
    replicateM n $ replicateM len rand
