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

-- Генерация случайных позиций (индексов) – используется только один раз
getRandomPositions :: RandomGen g => g -> Int -> (Int, Int) -> ([Int], g)
getRandomPositions gen 0 _ = ([], gen)
getRandomPositions gen n range =
    let (pos, gen') = randomR range gen
        (rest, gen'') = getRandomPositions gen' (n-1) range
    in (pos : rest, gen'')

-- Выбор генов по фиксированным позициям (чистая функция)
selectGenesByPositions :: [Int] -> Genotype -> [Gene]
selectGenesByPositions positions genotype = map (genotype !!) positions

-- Построение фенотипа и вычисление фитнеса для одного генотипа (чистая функция)
processChildFixed :: [Int] -> Phenotype -> Genotype -> ([Gene], Genotype, Double)
processChildFixed positions target childGenotype =
    let selected = selectGenesByPositions positions childGenotype
        phenotype = genotypeToPhenotype selected
        fitness = euclideanDistanceSquared target phenotype
    in (selected, childGenotype, fitness)

-- Обработка списка потомков (чистая функция)
processChildrenFixed :: [Int] -> Phenotype -> [Genotype] -> [([Gene], Genotype, Double)]
processChildrenFixed positions target children = map (processChildFixed positions target) children

-- Сохранение фенотипов детей в папку Children и возврат результатов
processingChildrenWithSave :: [Int] -> Phenotype -> Int -> [Genotype] -> IO [([Gene], Genotype, Double)]
processingChildrenWithSave positions target generation children = do
    let results = processChildrenFixed positions target children
    mapM_ (\(idx, (selected, _, _)) -> 
        saveChildPhenotype generation idx (genotypeToPhenotype selected)) (zip [0..] results)
    return results

saveChildPhenotype :: Int -> Int -> Phenotype -> IO ()
saveChildPhenotype generation childIndex phenotype =
    let filename = "Children/gen_" ++ show generation ++ "_child_" ++ show childIndex ++ ".png"
    in writePng filename phenotype

-- Основная функция эволюции
getEvolution :: RandomGen g => g -> Phenotype -> Genotype -> IO ()
getEvolution gen0 targetPhenotype initialParent = 
    let cnt_child = 20
        stagnationLimit = 10
        maxGenerations = 300

        -- ОДИН РАЗ генерируем 7 случайных позиций, которые будут использоваться везде
        (positions, gen0') = getRandomPositions gen0 7 (0,14)

        -- Начальный фитнес родителя с этими позициями
        initSelected = selectGenesByPositions positions initialParent
        initPhenotype = genotypeToPhenotype initSelected
        initFit = euclideanDistanceSquared targetPhenotype initPhenotype

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
                -- Здесь positions уже фиксированы (захвачены из let)
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
        putStrLn $ "Начальный фитнес родителя: " ++ show initFit
        putStrLn $ "Зафиксированы позиции для отбора генов: " ++ show positions
        loop gen0' 1 initialParent initSelected initFit 0