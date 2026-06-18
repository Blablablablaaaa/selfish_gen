module Main where

import Types
import Genetic
import Phenotype
import System.Random

getRandom7Genes :: RandomGen g => g -> Int -> (Int, Int) -> ([Int], g)
getRandom7Genes generator 0 _ = ([], generator)
getRandom7Genes generator n range =
    let (value, gen') = randomR range generator
        (values, gen'') = getRandom7Genes gen' (n-1) range
    in (value : values, gen'')

main :: IO ()
main = do
    gen <- getStdGen
    
    let (geno1, generator1) = randomGenotype gen
    putStrLn "Исходный генотип:"
    print geno1

    let (rand7gen, generator2) = getRandom7Genes generator1 7 (0, 14)

    -- Создаем и сохраняем биоморф
    let phenotype = genotypeToPhenotype rand7gen
    savePhenotype "test.png" phenotype
    
    putStrLn "Биоморф сохранен в test.png"