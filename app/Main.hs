module Main where

import Types
import Genetic
import Phenotype
import Fitness
import System.Random
import Codec.Picture
import Codec.Picture.Types
import System.Directory (createDirectoryIfMissing, copyFile)
import Data.List (minimumBy)

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

main :: IO ()
main = do
    createDirectoryIfMissing False "Children"
    
    -- Загружаем целевое изображение
    Right img <- readImage "kotic.png"
    let targetPhenotype = case img of
            ImageRGB8 rgb -> pixelMap (\(PixelRGB8 r g b) -> floor ((fromIntegral r + fromIntegral g + fromIntegral b) / 3)) rgb
            ImageRGBA8 rgba -> pixelMap (\(PixelRGBA8 r g b _) -> floor ((fromIntegral r + fromIntegral g + fromIntegral b) / 3)) rgba
            ImageY8 grey -> grey
            _ -> error "Неподдерживаемый формат изображения"
    
    putStrLn "Целевое изображение загружено"
    
    -- Генерируем родителя
    gen <- getStdGen
    let (parent, gen1) = randomGenotype gen
    
    putStrLn "\nРодительский генотип:"
    print parent
    
    -- Сохраняем родительский биоморф
    putStrLn "\nСохраняем родительский биоморф..."
    let (parentGenes, _) = getRandom7Genes gen1 parent
        parentPhenotype = genotypeToPhenotype parentGenes
    savePhenotype "Children/parent.png" parentPhenotype
    putStrLn "  Родитель сохранен как Children/parent.png"
    
    -- Генерируем 5 потомков
    let (children, gen2) = generatePopulation gen1 parent 5
    putStrLn $ "\nСгенерировано " ++ show (length children) ++ " потомков"
    
    putStrLn "\nОбработка потомков..."
    
    -- Обрабатываем каждого потомка
    let processChildren :: RandomGen g => g -> Int -> [Genotype] -> [(Genotype, Double, Int)] -> IO [(Genotype, Double, Int)]
        processChildren _ _ [] results = pure (reverse results)
        processChildren g idx (child:rest) results = do
            -- Берем 7 случайных генов из генотипа
            let (selectedGenes, g') = getRandom7Genes g child
                
                -- Создаем фенотип из выбранных генов
                phenotype = genotypeToPhenotype selectedGenes
                
                -- Вычисляем фитнес (сравниваем с целевым изображением)
                fitness = calculateFitness targetPhenotype selectedGenes
                
                filename = "Children/child_" ++ show idx ++ ".png"
            
            -- Сохраняем изображение
            savePhenotype filename phenotype
            putStrLn $ "  Потомок " ++ show idx ++ ": фитнес = " ++ show fitness
            
            -- Рекурсивно обрабатываем остальных
            processChildren g' (idx + 1) rest ((selectedGenes, fitness, idx) : results)
    
    results <- processChildren gen2 1 children []
    
    -- Находим лучшего (с минимальным фитнесом)
    let best = minimumBy (\(_, f1, _) (_, f2, _) -> compare f1 f2) results
        (bestGenes, bestFitness, bestIdx) = best
    
    putStrLn $ "\n=== РЕЗУЛЬТАТЫ ==="
    putStrLn $ "Лучший потомок: №" ++ show bestIdx
    putStrLn $ "Фитнес (расстояние): " ++ show bestFitness
    putStrLn $ "Гены лучшего: " ++ show bestGenes
    
    -- Копируем файл лучшего потомка как best.png
    copyFile ("Children/child_" ++ show bestIdx ++ ".png") "best.png"
    putStrLn "\nЛучший потомок сохранен как best.png"
    
    -- Показываем все результаты
    putStrLn "\nВсе результаты:"
    mapM_ (\(_, fitness, idx) -> 
        putStrLn $ "  Потомок " ++ show idx ++ ": " ++ show fitness
     ) results
    
    putStrLn "\nГотово! Проверьте папку Children/"