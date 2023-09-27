--module Tone_map where
import Files
import Elem3D


singleClamp :: Float -> RGB -> RGB
singleClamp x (RGB a b c) = (RGB a' b' c') where
    a' = if a > x then x else a
    b' = if b > x then x else b
    c' = if c > x then x else c


clamp :: Float -> [RGB] -> [RGB]
clamp x puntosRGB = puntosRGBClampeados where
    puntosRGBClampeados = map (singleClamp x) puntosRGB

singleEcualization :: RGB -> RGB
singleEcualization (RGB a b c) = RGB (a/255) (b/255) (c/255) 

ecualization :: [RGB] -> [RGB]
ecualization puntosRGB = puntosRGBEcualizados where
    puntosRGBEcualizados = map (singleEcualization) puntosRGB

singleEcualization' :: RGB -> RGB
singleEcualization' (RGB a b c) = RGB (a*255) (b*255) (c*255) 

ecualization' :: [RGB] -> [RGB]
ecualization' puntosRGB = puntosRGBEcualizados where
    puntosRGBEcualizados = map (singleEcualization') puntosRGB

-- Function to elevate RGB points to 1/X
elevateRGBPoints :: Float -> [RGB] -> [RGB]
elevateRGBPoints x points = map (elevateRGBPoint x) points
  where
    elevateRGBPoint :: Float -> RGB -> RGB
    elevateRGBPoint gamma (RGB r g b) =
        RGB (r ** (1.0 / x))
                 (g ** (1.0 / x))
                 (b ** (1.0 / x))

gammaFunc :: Float -> [RGB] -> [RGB]
gammaFunc x pixels = (ecualization' . (elevateRGBPoints x) . ecualization) pixels

main :: IO ()
main = do
  (pixels,(w,h)) <- leerPPM "./Images/forest_path.ppm"
  let modifiedPixels = (parsePixels' . ecualization . (clamp 10000)) pixels
  --putStrLn "Píxeles leídos:"
  --mapM_ print pixels
  writePPM "output.ppm" (round w) (round h) modifiedPixels
