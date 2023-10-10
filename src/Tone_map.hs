module Tone_map where
import Files
import Elem3D

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