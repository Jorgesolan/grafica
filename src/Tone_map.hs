module Tone_map where
import Files ()
import Elem3D ( RGB(..), elevateRGBPoint )

{-# INLINE clamp #-}
clamp :: Float -> [RGB] -> [RGB]
clamp x = map (\(RGB a b c) -> RGB (min x a) (min x b) (min x c))

{-# INLINE ecualization #-}
ecualization :: Float -> [RGB] -> [RGB]
ecualization x = map (\(RGB a b c) -> RGB (a / x) (b / x) (c / x))

{-# INLINE ecualization' #-}
ecualization' :: Float -> [RGB] -> [RGB]
ecualization' x = map (\(RGB a b c) -> RGB (a * x) (b * x) (c * x))

{-# INLINE elevateRGBPoints #-}
elevateRGBPoints :: Float -> [RGB] -> [RGB]
elevateRGBPoints x = map (elevateRGBPoint x)

{-# INLINE gammaFunc #-}
gammaFunc :: Float -> Float -> [RGB] -> [RGB]
gammaFunc x gamma = ecualization' 1 . elevateRGBPoints gamma . ecualization x
