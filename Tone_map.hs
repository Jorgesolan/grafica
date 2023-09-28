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
  (pixels,(w,h,fmx,pmax)) <- leerPPM "./Images/mpi_office.ppm"
  let modifiedPixels = (parsePixels'' . pixelReesclate (pmax/(fmx/3)) . clamp (fmx/3) ) pixels
  --putStrLn "Píxeles leídos:"
  --mapM_ print pixels
  putStrLn $ "Ancho (w): " ++ show w
  putStrLn $ "Alto (h): " ++ show h
  writePPM "output.ppm" (round w) (round h) modifiedPixels
