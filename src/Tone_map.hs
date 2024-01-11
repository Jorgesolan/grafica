{-# LANGUAGE RecordWildCards #-}
module Tone_map where
import Elem3D ( RGB(..), elevateRGBPoint )

{-# INLINE clamp #-}
-- |Función auxiliar, dado un valor máximo y una lista de RGBs, aplica la operación de clampeo con dicho valor sobre estos.
clamp :: Float -> [RGB] -> [RGB]
clamp x = map (\(RGB {..}) -> RGB (min x red) (min x green) (min x blue))

{-# INLINE ecualization #-}
-- |Función auxiliar, dado un valor y una lista de RGBs, aplica la operación de ecualización con dicho valor sobre estos.
ecualization :: Float -> [RGB] -> [RGB]
ecualization x = map (\(RGB {..}) -> RGB (red / x) (green / x) (blue / x))

{-# INLINE ecualization' #-}
-- |Función auxiliar, dado un valor y una lista de RGBs, revierte la operación de ecualización con dicho valor sobre estos.
ecualization' :: Float -> [RGB] -> [RGB]
ecualization' x = map (\(RGB {..}) -> RGB (red * x) (green * x) (blue * x))

{-# INLINE elevateRGBPoints #-}
-- |Función auxiliar, dado un valor y una lista de RGBs, eleva estos a 1/valor.
elevateRGBPoints :: Float -> [RGB] -> [RGB]
elevateRGBPoints x = map (elevateRGBPoint x)

{-# INLINE gammaFunc #-}
-- |Función auxiliar, dado dos valores y una lista de RGBs, aplica la función gamma con dichos valores sobre estos.
gammaFunc :: Float -> Float -> [RGB] -> [RGB]
gammaFunc x gamma = ecualization' 1 . elevateRGBPoints gamma . ecualization x

{-# INLINE gammaFunc' #-}
-- |Función auxiliar, dado dos valores y una lista de RGBs, revierte la aplicación de la función gamma con dichos valores sobre estos.
gammaFunc' :: Float -> Float -> [RGB] -> [RGB]
gammaFunc' x gamma = ecualization' 1 . elevateRGBPoints (1 / gamma) . ecualization x

