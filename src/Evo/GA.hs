module Evo.GA where

import Evo.Type

import System.Random.MWC
-- import Control.Monad.ST
import Control.Monad.State

decode :: Genotype -> Phenotype
decode = id

populate :: Int -> Int -> Evo s Population
populate len n = do
    replicateM n $ replicateM len rand

fitness :: Genotype -> Int
fitness = sum . map f
    where   f True = 1
            f False = 0


-- fitness-proportionate
select :: Population -> Evo s (Genotype, Genotype)
select pop = do
    a <- randR (0, fitnessSum - 1)
    b <- randR (0, fitnessSum - 1)
    return (at a pop, at b pop)
    where   fitnessSum = sum $ map fitness pop
            at n [] = error "population must be non-empty"
            at n (x:xs) | n < fitness x = x
                        | otherwise     = at (n - fitness x) xs


crossoverAt :: Int -> (Genotype, Genotype) -> (Genotype, Genotype)
crossoverAt n (a, b) = (a', b')
    where   a' = take n a ++ drop n b
            b' = take n b ++ drop n a

-- one point crossover
crossover :: Double -> (Genotype, Genotype) -> Evo s (Genotype, Genotype)
crossover p pair = do
    q <- randR (0, 1)
    if p < q then return pair
    else do
        n <- randR (0, length (fst pair) - 1)
        return (crossoverAt n pair)

mutateAt :: Int -> Genotype -> Genotype
mutateAt n gene = take n gene ++ [not (gene !! n)] ++ drop (n + 1) gene

mutate :: Double -> Genotype -> Evo s Genotype
mutate p [] = return []
mutate p (x:xs) = do
    q <- randR (0, 1)
    xs' <- mutate p xs
    if p < q
        then return (x : xs')
        else return (not x : xs')

unpair :: [(a, a)] -> [a]
unpair = foldr (\(a, b) xs -> a : b : xs) []
