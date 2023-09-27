--module Tone_map where
import Files
import Elem3D


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
  (pixels,(w,h)) <- leerPPM "./Images/forest_path.ppm"
  let modifiedPixels = (parsePixels' . (clamp 1000)) pixels
  --putStrLn "Píxeles leídos:"
  --mapM_ print pixels
  putStrLn $ "Ancho (w): " ++ show w
  putStrLn $ "Alto (h): " ++ show h
  writePPM "output.ppm" (round w) (round h) modifiedPixels
