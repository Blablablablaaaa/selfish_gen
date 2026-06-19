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
getRandomPositions gen count range = go gen count []
  where
    go g 0 acc = (acc, g)
    go g n acc =
        let (pos, g') = randomR range g
        in if pos `elem` acc
           then go g' n acc              -- дубликат — выбираем заново
           else go g' (n - 1) (pos : acc)

selectGenesByPositions :: [Int] -> Genotype -> [Gene]
selectGenesByPositions positions genotype = map (genotype !!) positions

processChild :: [Int] -> Phenotype -> Genotype -> ([Gene], Genotype, Double)
processChild positions target childGenotype =
    let selected = selectGenesByPositions positions childGenotype
        phenotype = genotypeToPhenotype selected
        fitness = euclideanDistanceSquared target phenotype
    in (selected, childGenotype, fitness)

processChildren :: [Int] -> Phenotype -> [Genotype] -> [([Gene], Genotype, Double)]
processChildren positions target children = map (processChild positions target) children

processingChildrenWithSave :: [Int] -> Phenotype -> Int -> [Genotype] -> IO [([Gene], Genotype, Double)]
processingChildrenWithSave positions target generation children = do
    let results = processChildren positions target children
    mapM_ (\(idx, (selected, _, _)) -> 
        saveChildPhenotype generation idx (genotypeToPhenotype selected)) (zip [0..] results)
    return results

saveChildPhenotype :: Int -> Int -> Phenotype -> IO ()
saveChildPhenotype generation childIndex phenotype =
    let filename = "Children/gen_" ++ show generation ++ "_child_" ++ show childIndex ++ ".png"
    in writePng filename phenotype

getEvolution :: RandomGen g => g -> Phenotype -> Genotype -> IO ()
getEvolution generator targetPhenotype firstParent = 
    let cnt_child = 20
        stagnationLimit = 10
        maxGenerations = 300

        (positions, geneartor1) = getRandomPositions generator 7 (0,14)

        parentSelectedGenes = selectGenesByPositions positions firstParent
        parentPhenotype = genotypeToPhenotype parentSelectedGenes
        parentFitness = euclideanDistanceSquared targetPhenotype parentPhenotype

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

                result_child <- processingChildrenWithSave positions targetPhenotype currentGen population

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

                loop gen' (currentGen + 1) bestFullGen bestSelectedGenes bestFitnessNew newStagnationCounter

    in do
        putStrLn $ "Начальный фитнес родителя: " ++ show parentFitness
        putStrLn $ "Зафиксированы позиции для отбора генов: " ++ show positions
        loop geneartor1 1 firstParent parentSelectedGenes parentFitness 0