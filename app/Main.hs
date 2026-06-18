module Main where

import Genetic
import Phenotype
import System.Random

getRandom7Genes :: RandomGen g => g -> Int -> (Int, Int) -> ([Int], g)
getRandom7Genes generator 0 _ = ([], generator)
getRandom7Genes generator n range =
    let (value, gen') = randomR range gen
        (values, gen'') = getRandom7Genes gen' (n-1) range
    in (value : values, gen'')

main :: IO ()
main = do
    putStrLn "=== Тест Genetics ==="
    
    -- Создаем генератор
    gen <- getStdGen
    
    -- Генерируем генотип
    let (geno1, gen1) = randomGenotype gen
    putStrLn "Исходный генотип:"
    print geno1
    
    -- Мутируем
    let (geno2, gen2) = mutateGenotype gen1 geno1
    putStrLn "Мутированный генотип:"
    print geno2
    
    -- Мутируем еще раз
    let (geno3, _) = mutateGenotype gen2 geno1
    putStrLn "Другой мутированный:"
    print geno3
    
    putStrLn "=== Тест завершен ==="