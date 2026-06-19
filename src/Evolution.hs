module Evolution (
    getEvolution
) where

import Types
import Genetic
import Phenotype
import Fitness
import Data.List (minimumBy)
import System.Random
import Codec.Picture (writePng)
import Control.Monad (mapM_, when)

getRandomPositions :: RandomGen g => g -> Int -> (Int, Int) -> ([Int], g)
getRandomPositions gen 0 _ = ([], gen)
getRandomPositions gen n range =
    let (pos, gen') = randomR range gen
        (rest, gen'') = getRandomPositions gen' (n-1) range
    in (pos : rest, gen'')

getRandom7Genes :: RandomGen g => g -> Genotype -> ([Int], g)
getRandom7Genes generator genotype =
    let (positions, gen') = getRandomPositions generator 7 (0, 14)
        selectedGenes = map (genotype !!) positions
    in (selectedGenes, gen')

saveChildPhenotype :: Int -> Int -> Phenotype -> IO ()
saveChildPhenotype generation childIndex phenotype =
    let filename = "Children/gen_" ++ show generation ++ "_child_" ++ show childIndex ++ ".png"
    in writePng filename phenotype

processingChild :: RandomGen g => g -> Phenotype -> Genotype -> ([Gene], Genotype, Double, g)
processingChild generator targetPhenotype child_genotype = 
    let (selectedGenes, gen1) = getRandom7Genes generator child_genotype
        phenotype = genotypeToPhenotype selectedGenes
        fitness = euclideanDistanceSquared targetPhenotype phenotype
    in (selectedGenes, child_genotype, fitness, gen1)

processingChildren :: RandomGen g => g -> Phenotype -> [Genotype] -> ([([Gene], Genotype, Double)], g)
processingChildren generator targetPhenotype [] = ([], generator)
processingChildren generator targetPhenotype (child:children) = 
    let (genes, fullGen, fitness, gen1) = processingChild generator targetPhenotype child
        (restResults, gen2) = processingChildren gen1 targetPhenotype children
        results = (genes, fullGen, fitness) : restResults
    in (results, gen2)

processingChildrenWithSave :: RandomGen g => g -> Phenotype -> Int -> [Genotype] -> IO ([([Gene], Genotype, Double)], g)
processingChildrenWithSave generator targetPhenotype generation children = do
    let (results, finalGen) = processingChildren generator targetPhenotype children
    mapM_ (\(idx, (selectedGenes, _, _)) -> saveChildPhenotype generation idx (genotypeToPhenotype selectedGenes)) (zip [0..] results)
    return (results, finalGen)

getEvolution :: RandomGen g => g -> Phenotype -> Genotype -> IO ()
getEvolution gen0 targetPhenotype initialParent = 
    let cnt_child = 20
        stagnationLimit = 10
        maxGenerations = 300

        loop :: RandomGen g => g -> Int -> Genotype -> [Gene] -> Double -> Int -> IO ()
        loop gen currentGen parent bestSelected bestFitness stagnationCounter
            | stagnationCounter >= stagnationLimit = do
                putStrLn "\n=== Эволюция остановлена: стагнация 10 поколений ==="
                let bestPhenotype = genotypeToPhenotype bestSelected
                    filename = "Best_last.png"
                writePng filename bestPhenotype
                putStrLn $ "Лучший фенотип сохранён как " ++ filename
                putStrLn $ "Фитнес: " ++ show bestFitness
            | currentGen > maxGenerations = do
                putStrLn "\n=== Достигнуто максимальное число поколений ==="
                let bestPhenotype = genotypeToPhenotype bestSelected
                    filename = "Best_last.png"
                writePng filename bestPhenotype
                putStrLn $ "Лучший фенотип сохранён как " ++ filename
                putStrLn $ "Фитнес: " ++ show bestFitness
            | otherwise = do
                let (population, gen') = generatePopulation gen parent cnt_child
                (result_child, gen'') <- processingChildrenWithSave gen' targetPhenotype currentGen population

                putStrLn $ "\n=== Поколение " ++ show currentGen ++ " ==="
                mapM_ (\(idx, (_, _, fitness)) ->
                    putStrLn $ "Ребёнок " ++ show idx ++ ": фитнес = " ++ show fitness
                    ) (zip [0..] result_child)

                let (bestSelectedGenes, bestFullGen, bestFitnessNew) = 
                        minimumBy (\(_, _, f1) (_, _, f2) -> compare f1 f2) result_child
                putStrLn $ "Лучший фитнес в поколении " ++ show currentGen ++ ": " ++ show bestFitnessNew

                let improvementThreshold = 0.999
                    improved = bestFitnessNew < bestFitness * improvementThreshold
                    newStagnationCounter = if improved then 0 else stagnationCounter + 1

                when (stagnationCounter > 0 && improved) $
                    putStrLn "Стагнация прервана!"

                loop gen'' (currentGen + 1) bestFullGen bestSelectedGenes bestFitnessNew newStagnationCounter

    in do
        let (initSelected, initFull, initFit, gen1) = processingChild gen0 targetPhenotype initialParent
        putStrLn $ "Начальный фитнес родителя: " ++ show initFit
        loop gen1 1 initFull initSelected initFit 0
