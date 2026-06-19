module Fitness (
    euclideanDistanceSquared
) where

import Types
import Phenotype
import Codec.Picture
import Codec.Picture.Types
import Data.List (minimumBy)

toList :: Image Pixel8 -> [Pixel8]
toList img = [ pixelAt img x y | y <- [0..imageHeight img - 1], x <- [0..imageWidth img - 1] ]

euclideanDistanceSquared :: Phenotype -> Phenotype -> Double
euclideanDistanceSquared img1 img2 =
    let pixels1 = toList img1
        pixels2 = toList img2

        diffSquared p1 p2 = 
            let diff = fromIntegral p1 - fromIntegral p2
            in diff * diff

        sumSquares = sum (zipWith diffSquared pixels1 pixels2)
    in fromIntegral sumSquares

-- calculateFitness :: Phenotype -> Genotype -> Double
-- calculateFitness target genotype =
--     let phenotype = genotypeToPhenotype genotype
--     in euclideanDistanceSquared target phenotype

-- compareToTarget :: Phenotype -> [Genotype] -> [(Genotype, Double)]
-- compareToTarget target genotypes =
--     map (\g -> (g, calculateFitness target g)) genotypes

-- selectBest :: Phenotype -> [Genotype] -> (Genotype, Double)
-- selectBest target genotypes =
--     let withFitness = compareToTarget target genotypes
--         best = minimumBy (\(_, f1) (_, f2) -> compare f1 f2) withFitness
--     in best

