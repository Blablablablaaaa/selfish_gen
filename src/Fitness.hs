module Fitness (
    euclideanDistanceSquared
) where

import Types
import Codec.Picture

toList :: Image Pixel8 -> [Pixel8]
toList img = [ pixelAt img x y | y <- [0..imageHeight img - 1], x <- [0..imageWidth img - 1] ]

euclideanDistanceSquared :: Phenotype -> Phenotype -> Double
euclideanDistanceSquared img1 img2
    | imageWidth img1 /= imageWidth img2 || imageHeight img1 /= imageHeight img2 =
        error "euclideanDistanceSquared: размеры изображений не совпадают"
    | otherwise =
        let pixels1 = toList img1
            pixels2 = toList img2

            diffSquared p1 p2 =
                let diff = fromIntegral p1 - fromIntegral p2
                in diff * diff

            sumSquares = sum (zipWith diffSquared pixels1 pixels2)
        in fromIntegral sumSquares

