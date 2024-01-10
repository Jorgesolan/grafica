{-# LANGUAGE RecordWildCards #-}
module Tone_map where
import Elem3D ( RGB(..), elevateRGBPoint )

{-# INLINE clamp #-}
clamp :: Float -> [RGB] -> [RGB]
clamp x = map (\(RGB {..}) -> RGB (min x red) (min x green) (min x blue))

{-# INLINE ecualization #-}
ecualization :: Float -> [RGB] -> [RGB]
ecualization x = map (\(RGB {..}) -> RGB (red / x) (green / x) (blue / x))

{-# INLINE ecualization' #-}
ecualization' :: Float -> [RGB] -> [RGB]
ecualization' x = map (\(RGB {..}) -> RGB (red * x) (green * x) (blue * x))

{-# INLINE elevateRGBPoints #-}
elevateRGBPoints :: Float -> [RGB] -> [RGB]
elevateRGBPoints x = map (elevateRGBPoint x)

{-# INLINE gammaFunc #-}
gammaFunc :: Float -> Float -> [RGB] -> [RGB]
gammaFunc x gamma = ecualization' 1 . elevateRGBPoints gamma . ecualization x

{-# INLINE gammaFunc' #-}
gammaFunc' :: Float -> Float -> [RGB] -> [RGB]
gammaFunc' x gamma = ecualization' 1 . elevateRGBPoints (1 / gamma) . ecualization x

