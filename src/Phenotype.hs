module Phenotype (
    genotypeToPhenotype,
    savePhenotype
) where

import Types
import System.Random
import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Drawing
import Data.Word (Word8)

directionVector :: Direction -> (Int, Int)
directionVector N  = (0, -1)    -- вверх
directionVector NE = (1, 1)    -- вверх-вправо
directionVector E  = (1, 0)    -- вправо
directionVector SE = (1, -1)   -- вниз-вправо
directionVector S  = (0, 1)   -- вниз
directionVector SW = (-1, -1)  -- вниз-влево
directionVector W  = (-1, 0)   -- влево
directionVector NW = (-1, 1)   -- вверх-влево

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
buildMirrorVectors size segments = map (buildMirrorVector size) segments
    where
        buildMirrorVector width ((x1, y1), (x2, y2)) = 
            ((width - x1, y1), (width - x2, y2))

drawVector :: MutableImage PixelRGB8 -> [((Int, Int), (Int, Int))] -> PixelRGB8 -> MutableImage PixelRGB8
drawVector canvas [] _ = canvas
drawVector canvas ((start, end):rest) color = 
    let (x1, y1) = start
        (x2, y2) = end

        drawn = drawLine canvas x1 y1 x2 y2 color

    in drawVector drawn rest color

genotypeToPhenotype :: Genotype -> Phenotype
genotypeToPhenotype genotype = 
    let directions = map genToDirections genotype
        
        stepLen = 10
        
        segments = buildVector (75, 75) directions stepLen

        size = 150
        
        allSegments = segments ++ buildMirrorVectors size segments
        
        whitePixel = PixelRGB8 255 255 255
        canvas = createMutableImage 150 150 whitePixel
        
        blackPixel = PixelRGB8 0 0 0
        drawn = drawVector canvas allSegments blackPixel
        
    in freezeImage drawn

savePhenotype :: FilePath -> Phenotype -> IO ()
savePhenotype filename image = do
    let rgbImage = pixelMap (\p -> PixelRGB8 p p p) image
    savePngImage filename (ImageRGB8 rgbImage)
