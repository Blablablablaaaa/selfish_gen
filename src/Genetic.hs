module Genetic (
    randomGenotype,
    randomMask,
    mutateGenotype,
    generatePopulation
) where

import Types
import System.Random

-- Выбираем 7 РАЗНЫХ случайных позиций из [0..14] — это «маска» генов,
-- которые участвуют в построении скелета. Выбирается ОДИН раз за запуск.
randomMask :: RandomGen g => g -> ([Int], g)
randomMask gen = go gen 7 []
  where
    go g 0 acc = (acc, g)
    go g n acc =
        let (pos, g') = randomR (0, 14) g
        in if pos `elem` acc
           then go g' n acc          -- дубликат — пробуем ещё раз
           else go g' (n - 1) (pos : acc)

generateGenes :: RandomGen g => g -> Int -> (Int, Int) -> ([Int], g)
generateGenes generator 0 _ = ([], generator)
generateGenes gen n range =
    let (value, gen') = randomR range gen
        (values, gen'') = generateGenes gen' (n-1) range
    in (value : values, gen'')

randomGenotype :: RandomGen g => g -> (Genotype, g)
randomGenotype generator =
        let (genotip1_15, generator1) = generateGenes generator 15 (-9,9)
            (length_gen, generator2) = randomR (2, 12) generator1
        in (genotip1_15 ++ [length_gen], generator2)

mutateGen :: RandomGen g => g -> Gene -> Int -> (Gene, g)
mutateGen gen gene pos
    | pos == 15 =
        let (direction, gen') = randomR (False, True) gen
            newVal = if direction
                     then max 2 (gene - 1)  
                     else min 12 (gene + 1) 
        in (newVal, gen')
    | otherwise =  
        let (direction, gen') = randomR (False, True) gen
            newVal = if direction
                     then max (-9) (gene - 1)  
                     else min 9 (gene + 1)     
        in (newVal, gen')

replaceAt :: Int -> a -> [a] -> [a]
replaceAt pos newVal list =
        let (before, _:after) = splitAt pos list  
        in before ++ [newVal] ++ after

mutateGenotype :: RandomGen g => g -> Genotype -> (Genotype, g)
mutateGenotype generator genotype =
    let (pos, generator1) = randomR (0, 15) generator
        oldGene = genotype !! pos 
        (newGene, generator2) = mutateGen generator1 oldGene pos
        newGenotype = replaceAt pos newGene genotype
    in (newGenotype, generator2)

generatePopulation :: RandomGen g => g -> Genotype -> Int -> ([Genotype], g)
generatePopulation generator _ 0 = ([], generator)
generatePopulation generator parent population_size = 
    let (genotype, generator1) = mutateGenotype generator parent
        (populations, generator2) = generatePopulation generator1 parent (population_size-1)
    in (genotype : populations, generator2)
