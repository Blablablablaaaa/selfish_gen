module Main where

import Genetic
import Evolution 
import System.Random
import Codec.Picture 
import Codec.Picture.Types ()
import System.Directory (createDirectoryIfMissing, removeFile, getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)

clearDirectory :: FilePath -> IO ()
clearDirectory dir = do
    createDirectoryIfMissing True dir
    contents <- getDirectoryContents dir
    forM_ contents $ \f -> do
        let path = dir </> f
        isFile <- doesFileExist path
        when isFile $ removeFile path

main :: IO ()
main = do

    -- 1. Очистить папку Children
    clearDirectory "Children"
    
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

    -- Выбираем 7 случайных генов из 15 ОДИН раз — маска фиксируется на весь запуск
    let (mask, gen2) = randomMask gen1
    putStrLn "\nМаска (позиции 7 выбранных генов):"
    print mask

    getEvolution gen2 mask targetPhenotype parent
    putStrLn "\nГотово."
