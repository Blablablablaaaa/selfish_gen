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

-- Получить 7 случайных позиций (индексов) от 0 до 14
getRandomPositions :: RandomGen g => g -> Int -> (Int, Int) -> ([Int], g)
getRandomPositions gen 0 _ = ([], gen)
getRandomPositions gen n range =
    let (pos, gen') = randomR range gen
        (rest, gen'') = getRandomPositions gen' (n-1) range
    in (pos : rest, gen'')

-- Получить 7 случайных генов из генотипа
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

-- processingChildren ОБЁРТКА с IO для сохранения
processingChildrenWithSave :: RandomGen g => g -> Phenotype -> Int -> [Genotype] -> IO ([([Gene], Genotype, Double)], g)
processingChildrenWithSave generator targetPhenotype generation children = do
    let (results, finalGen) = processingChildren generator targetPhenotype children
    -- Сохраняем фенотип каждого ребёнка (используем selectedGenes)
    mapM_ (\(idx, (selectedGenes, _, _)) -> saveChildPhenotype generation idx (genotypeToPhenotype selectedGenes)) (zip [0..] results)
    return (results, finalGen)

getEvolution :: RandomGen g => g -> Phenotype -> Genotype -> Int -> IO (Genotype, Double)
getEvolution generator targetPhenotype parent_genotype 0 = do
    -- базовый случай, который не должен вызываться при cnt_pokolenyi >= 1,
    -- но на всякий случай вычислим фитнес для переданного генотипа
    let (_, _, fitness, _) = processingChild generator targetPhenotype parent_genotype
    putStrLn "Эволюция завершена."
    return (parent_genotype, fitness)

getEvolution generator targetPhenotype parent_genotype cnt_pokolenyi = do
    let cnt_child = 10
        (population, gen1) = generatePopulation generator parent_genotype cnt_child
    (result_child, gen2) <- processingChildrenWithSave gen1 targetPhenotype cnt_pokolenyi population

    -- Вывод всех фитнесов текущего поколения
    putStrLn $ "\n=== Поколение " ++ show cnt_pokolenyi ++ " ==="
    mapM_ (\(idx, (_, _, fitness)) ->
        putStrLn $ "Ребёнок " ++ show idx ++ ": фитнес = " ++ show fitness
        ) (zip [0..] result_child)

    let (_, bestFullGen, bestFitness) = 
            minimumBy (\(_, _, f1) (_, _, f2) -> compare f1 f2) result_child
    putStrLn $ "Лучший фитнес в поколении " ++ show cnt_pokolenyi ++ ": " ++ show bestFitness

    -- Если это последнее поколение, которое требовалось обработать, возвращаем его лучшего
    if cnt_pokolenyi == 1
        then return (bestFullGen, bestFitness)
        else getEvolution gen2 targetPhenotype bestFullGen (cnt_pokolenyi - 1)
