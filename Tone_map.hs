--module Tone_map where
import Files
import Elem3D
-- ghc Elem3D.hs Files.hs Tone_map.hs -outputdir ./bin && ./Tone_map
-- ghc Elem3D.hs Files.hs Tone_map.hs -outputdir ./bin -rtsopts -O2 && ./Tone_map
singleClamp :: Float -> RGB -> RGB
singleClamp x (RGB a b c) =
    RGB (min x a) (min x b) (min x c)

singleEcualization :: RGB -> RGB
singleEcualization (RGB a b c) =
    RGB (a / 255) (b / 255) (c / 255)

singleEcualization' :: RGB -> RGB
singleEcualization' (RGB a b c) =
    RGB (a * 255) (b * 255) (c * 255)

elevateRGBPoint :: Float -> RGB -> RGB
elevateRGBPoint x (RGB r g b) =
    RGB (r ** (1.0 / x))
        (g ** (1.0 / x))
        (b ** (1.0 / x))

clamp :: Float -> [RGB] -> [RGB]
clamp x = map (singleClamp x)

ecualization :: [RGB] -> [RGB]
ecualization = map singleEcualization

ecualization' :: [RGB] -> [RGB]
ecualization' = map singleEcualization'

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
    
    -- let path = "mpi_office"; gamma = 6.0
    (pixels,(w,h)) <- leerPPM ("./Images/" ++ path ++ ".ppm")
    let a = (pixels2BMP.(gammaFunc gamma)) pixels
    writeBMP  ("./tmp/" ++ path ++".bmp") (round w) (round h) a
    --let !b = (parsePixels' (round w)) a
    --writePPM "output.ppm" (round w) (round h) b