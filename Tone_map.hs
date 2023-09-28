--module Tone_map where
import Files
import Elem3D

elevateRGBPoint :: Float -> RGB -> RGB
elevateRGBPoint x (RGB r g b) =
    RGB (r ** (1.0 / x))
        (g ** (1.0 / x))
        (b ** (1.0 / x))

clamp :: Float -> [RGB] -> [RGB]
clamp x = map (\(RGB a b c) -> RGB (min x a) (min x b) (min x c))

ecualization :: [RGB] -> [RGB]
ecualization = map (\(RGB a b c) -> RGB (a / 255) (b / 255) (c / 255))

ecualization' :: [RGB] -> [RGB]
ecualization' = map (\(RGB a b c) -> RGB (a * 255) (b * 255) (c * 255))

elevateRGBPoints :: Float -> [RGB] -> [RGB]
elevateRGBPoints x = map (elevateRGBPoint x)

gammaFunc :: Float -> [RGB] -> [RGB]
gammaFunc x = (ecualization' . (elevateRGBPoints x) . ecualization)

main :: IO ()
main = do
    -- let path = "forest_path"; gamma = 4.0
    
    let path = "seymour_park"; gamma = 10.0

    -- let path = "mpi_atrium_1"; gamma = 3.0

    -- let path = "nancy_church_3"; gamma = 4.0

--      (pixels,(w,h,fmx,pmax)) <- leerPPM "./Images/mpi_office.ppm"
--   let modifiedPixels = (parsePixels'' . pixelReesclate (pmax/(fmx/3)) . clamp (fmx/3) ) pixels
  
    
    -- let path = "mpi_office"; gamma = 6.0
    (pixels,(w,h)) <- leerPPM ("./Images/" ++ path ++ ".ppm")
    let a = (pixels2BMP.(gammaFunc gamma)) pixels
    writeBMP  ("./tmp/" ++ path ++".bmp") (round w) (round h) a
    --let !b = (parsePixels' (round w)) a
    --writePPM "output.ppm" (round w) (round h) b