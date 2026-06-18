-- src/Evolution/Fitness.hs
module Evolution.Fitness (
    euclideanDistanceSquared,
    calculateFitness,
    compareToTarget
) where

import Types (Phenotype, Genotype, genotypeToPhenotype)
import Codec.Picture
import Codec.Picture.Types

-- Евклидово расстояние без корня (сумма квадратов разностей)
euclideanDistanceSquared :: Phenotype -> Phenotype -> Double
euclideanDistanceSquared img1 img2 =
    let -- Проверяем, что размеры совпадают
        w1 = imageWidth img1
        h1 = imageHeight img1
        w2 = imageWidth img2
        h2 = imageHeight img2
        
        -- Если размеры разные, возвращаем максимальное значение
        -- (это значит, что картинки не совместимы)
    in if w1 /= w2 || h1 /= h2
       then 1.0 / 0.0  -- бесконечность (очень плохо)
       else
           let -- Превращаем изображения в списки пикселей
               pixels1 = toList img1
               pixels2 = toList img2
               
               -- Функция: разность квадратов для пары пикселей
               diffSquared p1 p2 = 
                   let diff = fromIntegral p1 - fromIntegral p2
                   in diff * diff
               
               -- Суммируем квадраты разностей
               sumSquares = sum (zipWith diffSquared pixels1 pixels2)
               
           -- Возвращаем сумму квадратов (без корня!)
           in fromIntegral sumSquares

-- Вычисление фитнеса для одного генотипа
calculateFitness :: Phenotype -> Genotype -> Double
calculateFitness target genotype =
    let phenotype = genotypeToPhenotype genotype
    in euclideanDistanceSquared target phenotype

-- Сравнение всех генотипов с целевым изображением
compareToTarget :: Phenotype -> [Genotype] -> [(Genotype, Double)]
compareToTarget target genotypes =
    map (\g -> (g, calculateFitness target g)) genotypes

-- Получение лучшего генотипа (с наименьшим расстоянием)
selectBest :: Phenotype -> [Genotype] -> (Genotype, Double)
selectBest target genotypes =
    let -- Вычисляем фитнес для всех
        withFitness = compareToTarget target genotypes
        -- Находим минимальный (лучший)
        best = minimumBy (\(_, f1) (_, f2) -> compare f1 f2) withFitness
    in best

-- Вспомогательная функция: изображение в список пикселей
toList :: Image Pixel8 -> [Pixel8]
toList img = [ pixelAt img x y | y <- [0..imageHeight img - 1], x <- [0..imageWidth img - 1] ]