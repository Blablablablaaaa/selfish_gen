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
    
    -- Генерируем родительский генотип
    let (parentGeno, generator1) = randomGenotype gen
    putStrLn "Родительский генотип:"
    print parentGeno
    
    -- Выбираем 7 генов для построения родителя
    let (rand7gen, generator2) = getRandom7Genes generator1 7 (0, 14)
    
    -- Сохраняем родителя
    let parentPhenotype = genotypeToPhenotype rand7gen
    savePhenotype "parent.png" parentPhenotype
    putStrLn "Родитель сохранен в parent.png"
    
    -- Генерируем 5 потомков
    let (genotypes, generator3) = generatePopulation generator2 parentGeno 5
    
    -- Сохраняем каждого потомка с уникальными 7 генами
    let saveAll :: RandomGen g => g -> Int -> [Genotype] -> IO ()
        saveAll _ _ [] = pure ()
        saveAll gen idx (geno:rest) = do
            let (randGenes, newGen) = getRandom7Genes gen 7 (0, 14)
                phenotype = genotypeToPhenotype randGenes
                filename = "Children/child_" ++ show idx ++ ".png"
            savePhenotype filename phenotype
            putStrLn $ "Потомок " ++ show idx ++ " сохранен в " ++ filename
            saveAll newGen (idx + 1) rest
    
    saveAll generator3 1 genotypes
    
    putStrLn "\nГотово! Проверьте папку Children."