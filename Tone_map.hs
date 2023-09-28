--module Tone_map where
import Files
import Elem3D
import System.CPUTime

elevateRGBPoint :: Float -> RGB -> RGB
elevateRGBPoint x (RGB r g b) =
    RGB (r ** (1.0 / x))
        (g ** (1.0 / x))
        (b ** (1.0 / x))

clamp :: Float -> [RGB] -> [RGB]
clamp x = map (\(RGB a b c) -> RGB (min x a) (min x b) (min x c))

ecualization :: Float -> [RGB] -> [RGB]
ecualization x = map (\(RGB a b c) -> RGB (a / x) (b / x) (c / x))

ecualization' :: Float -> [RGB] -> [RGB]
ecualization' x = map (\(RGB a b c) -> RGB (a * x) (b * x) (c * x))

elevateRGBPoints :: Float -> [RGB] -> [RGB]
elevateRGBPoints x = map (elevateRGBPoint x)

gammaFunc :: Float -> Float -> [RGB] -> [RGB]
gammaFunc x gamma = (ecualization' 255) . (elevateRGBPoints gamma) . (ecualization x)

main :: IO ()
main = do
    start <- getCPUTime
    -- let path = "forest_path"; gamma = 4.0
    
    let path = "mpi_atrium_1"; gamma = 3.78

    -- let path = "mpi_atrium_1"; gamma = 3.0

    -- let path = "nancy_church_3"; gamma = 4.0

    --   let modifiedPixels = (parsePixels'' . pixelReesclate (pmax/(fmx/3)) . clamp (fmx/3) ) pixels
    
    -- let path = "mpi_office"; gamma = 6.0
    (pixels,(w,h,fmx,pmx)) <- leerPPM ("./Images/" ++ path ++ ".ppm")
    let a = (pixels2BMP. (gammaFunc fmx gamma) ) pixels
    writeBMP  ("./tmp/" ++ path ++".bmp") (round w) (round h) a
    let b = (parsePixels . (gammaFunc fmx gamma) ) pixels
    writePPM ("./tmp/" ++ path ++".ppm") (round w) (round h) b
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) :: Double
    putStrLn $ "Tiempo de ejecución: " ++ show diff ++ " segundos"