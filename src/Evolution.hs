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

saveChildPhenotype :: Int -> Int -> Phenotype -> IO ()
saveChildPhenotype generation childIndex phenotype =
    let filename = "Children/gen_" ++ show generation ++ "_child_" ++ show childIndex ++ ".png"
    in writePng filename phenotype

-- Строим фенотип ребёнка из его полного генотипа и считаем фитнес
processingChild :: Phenotype -> Genotype -> (Genotype, Double)
processingChild targetPhenotype child_genotype =
    let phenotype = genotypeToPhenotype child_genotype
        fitness = euclideanDistanceSquared targetPhenotype phenotype
    in (child_genotype, fitness)

processingChildren :: Phenotype -> [Genotype] -> [(Genotype, Double)]
processingChildren targetPhenotype = map (processingChild targetPhenotype)

-- processingChildren ОБЁРТКА с IO для сохранения фенотипов
processingChildrenWithSave :: Phenotype -> Int -> [Genotype] -> IO [(Genotype, Double)]
processingChildrenWithSave targetPhenotype generation children = do
    let results = processingChildren targetPhenotype children
    mapM_ (\(idx, (genotype, _)) -> saveChildPhenotype generation idx (genotypeToPhenotype genotype))
          (zip [0..] results)
    return results

getEvolution :: RandomGen g => g -> Phenotype -> Genotype -> IO ()
getEvolution gen0 targetPhenotype initialParent =
    let cnt_child = 20
        stagnationLimit = 10
        maxGenerations = 10000

        saveBest :: Genotype -> Double -> IO ()
        saveBest bestGenotype bestFitness = do
            let filename = "Best_last.png"
            writePng filename (genotypeToPhenotype bestGenotype)
            putStrLn $ "Лучший фенотип сохранён как " ++ filename
            putStrLn $ "Фитнес: " ++ show bestFitness

        -- parent / parentFitness — текущий родитель;
        -- bestGenotype / bestFitness — глобально лучший за всю эволюцию.
        loop :: RandomGen g => g -> Int -> Genotype -> Double -> Genotype -> Double -> Int -> IO ()
        loop gen currentGen parent parentFitness bestGenotype bestFitness stagnationCounter
            | stagnationCounter >= stagnationLimit = do
                putStrLn "\n=== Эволюция остановлена: стагнация 10 поколений ==="
                saveBest bestGenotype bestFitness
            | currentGen > maxGenerations = do
                putStrLn "\n=== Достигнуто максимальное число поколений ==="
                saveBest bestGenotype bestFitness
            | otherwise = do
                let (population, gen') = generatePopulation gen parent cnt_child
                result_child <- processingChildrenWithSave targetPhenotype currentGen population

                putStrLn $ "\n=== Поколение " ++ show currentGen ++ " ==="
                mapM_ (\(idx, (_, fitness)) ->
                    putStrLn $ "Ребёнок " ++ show idx ++ ": фитнес = " ++ show fitness
                    ) (zip [0..] result_child)

                let (genBest, genBestFitness) =
                        minimumBy (\(_, f1) (_, f2) -> compare f1 f2) result_child
                putStrLn $ "Лучший фитнес в поколении " ++ show currentGen ++ ": " ++ show genBestFitness

                -- Сравниваем с глобально лучшим, а не с прошлым поколением
                let improvementThreshold = 0.999
                    improved = genBestFitness < bestFitness * improvementThreshold
                    newStagnationCounter = if improved then 0 else stagnationCounter + 1
                    (newBestGenotype, newBestFitness) =
                        if genBestFitness < bestFitness
                        then (genBest, genBestFitness)
                        else (bestGenotype, bestFitness)

                when (stagnationCounter > 0 && improved) $
                    putStrLn "Стагнация прервана!"

                -- Лучший ребёнок поколения становится родителем следующего
                loop gen' (currentGen + 1) genBest genBestFitness
                     newBestGenotype newBestFitness newStagnationCounter

    in do
        let (_, initFit) = processingChild targetPhenotype initialParent
        putStrLn $ "Начальный фитнес родителя: " ++ show initFit
        loop gen0 1 initialParent initFit initialParent initFit 0
