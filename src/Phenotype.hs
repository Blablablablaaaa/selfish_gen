module Phenotype (
    genotypeToPhenotype
) where

import Types
import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Drawing
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Word (Word8)

directionVector :: Direction -> (Int, Int)
directionVector N  = (0, -1)    -- вверх
directionVector NE = (1, 1)     -- вверх-вправо
directionVector E  = (1, 0)     -- вправо
directionVector SE = (1, -1)    -- вниз-вправо
directionVector S  = (0, 1)     -- вниз
directionVector SW = (-1, -1)   -- вниз-влево
directionVector W  = (-1, 0)    -- влево
directionVector NW = (-1, 1)    -- вверх-влево

genToDirections :: Gene -> Direction
genToDirections gene = toEnum ((gene + 9) `mod` 8)

buildVector :: (Int, Int) -> [Direction] -> Int -> [((Int, Int), (Int, Int))]
buildVector start [] _ = []
buildVector start (dir : directs) len = 
    let (dx, dy) = directionVector dir
        next = (fst start + dx * len, snd start + dy * len)
        segment = (start, next)
        restSegments = buildVector next directs len
    in segment : restSegments

buildMirrorVectors :: Int -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
buildMirrorVectors size segments = map buildMirrorVector segments
    where
        buildMirrorVector ((x1, y1), (x2, y2)) = 
            ((size - 1 - x1, y1), (size - 1 - x2, y2))

drawSegments :: (PrimMonad m) => MutableImage (PrimState m) Pixel8 -> [((Int, Int), (Int, Int))] -> Pixel8 -> m ()
drawSegments canvas [] _ = pure ()
drawSegments canvas ((start, end):rest) color = do
    let (x1, y1) = start
        (x2, y2) = end
    drawLine canvas x1 y1 x2 y2 color
    drawSegments canvas rest color

genotypeToPhenotype :: Genotype -> Phenotype
genotypeToPhenotype genotype = 
    let directions = map genToDirections genotype
        stepLen = 10
        segments = buildVector (75, 75) directions stepLen
        size = 150
        allSegments = segments ++ buildMirrorVectors size segments
        whitePixel = 255 :: Pixel8  -- белый фон
        blackPixel = 0 :: Pixel8    -- черные линии
    in runST $ do
        canvas <- createMutableImage 150 150 whitePixel
        drawSegments canvas allSegments blackPixel
        freezeImage canvas

