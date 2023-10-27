module Elem3D where
import Control.DeepSeq
import System.IO ()
{-# LANGUAGE BangPatterns #-}

data Point3D = Point3D !Float !Float !Float
data Direction = Direction !Float !Float !Float
data Ray = Ray !Point3D !Direction Float
data Base = Base Direction Direction Direction
data RGB = RGB { red :: !Float, green :: !Float, blue :: !Float } deriving (Show)
data Luz = Luz !Point3D !RGB !Float

-- instance NFData RGB where
--     rnf (RGB r g b) = rnf r `seq` rnf g `seq` rnf b
-- instance NFData Point3D where
--     rnf (Point3D x y z) = rnf x `seq` rnf y `seq` rnf z
-- instance NFData Direction where
--     rnf (Direction x y z) = rnf x `seq` rnf y `seq` rnf z
-- instance NFData Ray where
--     rnf (Ray p d f) = rnf p `seq` rnf d `seq` rnf f


instance Show Base where
    show (Base (Direction dx0 dy0 dz0) (Direction dx1 dy1 dz1) (Direction dx2 dy2 dz2)) = "Base:\n| " ++ show dx0 ++" " ++show dy0 ++" "++ show dz0 ++ " |\n" ++ "| " ++ show dx1 ++" " ++show dy1 ++" "++ show dz1 ++ " |\n"++ "| " ++ show dx2 ++" " ++show dy2 ++" "++ show dz2 ++ "| "

instance Show Point3D where
    show (Point3D x y z) = "Point3D " ++ show x ++ " " ++ show y ++ " " ++ show z

instance Show Direction where
    show (Direction dx dy dz) = "Direction " ++ show dx ++ " " ++ show dy ++ " " ++ show dz

instance Show Ray where
    show (Ray p d _) = "Rayo desde " ++ show p ++ " hasta " ++ show d

roundTo :: Int -> Point3D -> Point3D
roundTo n (Point3D a b c) = Point3D a' b' c'
    where
        a' = (fromInteger $ round $ a * (10^n)) / (10.0^^n)::Float
        b' = (fromInteger $ round $ b * (10^n)) / (10.0^^n)::Float
        c' = (fromInteger $ round $ c * (10^n)) / (10.0^^n)::Float


-- ************************************************
-- Angulos y transformaciones
radToDeg :: Float -> Float
radToDeg radians = radians * (180.0 / pi)

degToRad :: Float -> Float
degToRad degree = degree * (pi / 180.0)

normalizeAngle :: Float -> Float
normalizeAngle f = f - (2*pi * floor' (f / (2*pi)))
    where floor' :: Float -> Float
          floor' x = fromIntegral (floor x :: Int)
-- ************************************************

rotatePoint :: Char -> Float -> Point3D -> Point3D
rotatePoint axis degree  (Point3D x y z)
 | axis == 'X' = Point3D x (c*y + s*z) (-s*y + c*z)
 | axis == 'Y' = Point3D (-s*z + c*x) y (s*x + c*z)
 | axis == 'Z' = Point3D (c*x + s*y) (-s*x + c*y) z
 | otherwise = Point3D 0 0 0
    where radiant = normalizeAngle $ (degToRad degree)
          c = cos radiant
          s = sin radiant

rotatePoint' :: Char -> Float -> Point3D -> Point3D
rotatePoint' axis degree  (Point3D x y z)
 | axis == 'X' = Point3D x (c*y + s*z) (-s*y + c*z)
 | axis == 'Y' = Point3D (-s*z + c*x) y (s*x + c*z)
 | axis == 'Z' = Point3D (c*x + s*y) (-s*x + c*y) z
 | otherwise = Point3D 0 0 0
    where radiant = normalizeAngle $ degToRad (360 - degree)
          c = cos radiant
          s = sin radiant

{-# INLINE movePoint #-}
movePoint :: Direction -> Point3D -> Point3D
movePoint (Direction dx dy dz) (Point3D x y z) = Point3D (x + dx) (y + dy) (z + dz)

{-# INLINE movePoint' #-}
movePoint' :: Direction -> Point3D -> Point3D
movePoint' (Direction dx dy dz) (Point3D x y z) = Point3D (x - dx) (y - dy) (z - dz)

{-# INLINE distPoint #-}
distPoint :: Point3D -> Point3D -> Float
distPoint (Point3D x1 y1 z1) (Point3D x2 y2 z2) = sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)

-- -- Resta de puntos -> Dirección del primero al segundo
{-# INLINE (#) #-}
(#) :: Point3D -> Point3D -> Point3D
(Point3D xb yb zb) # (Point3D xa ya za) = Point3D (xb-xa) (yb-ya) (zb-za)

-- -- Direccion entre puntos -> Dirección del primero al segundo
{-# INLINE (#<) #-}
(#<) :: Point3D -> Point3D -> Direction
(Point3D xb yb zb) #< (Point3D xa ya za) = Direction (xb-xa) (yb-ya) (zb-za)

{-# INLINE aproxPoint #-}
aproxPoint :: Point3D -> Point3D -> Bool
aproxPoint (Point3D xb yb zb) (Point3D xa ya za) = a && b && c
    where
        a = abs (xb-xa) < 0.1
        b = abs (yb-ya) < 0.1
        c = abs (zb-za) < 0.1

instance Num Direction where
-- Suma de direcciones
--  (+) :: Direction -> Direction -> Direction
 {-# INLINE (+) #-}
 (Direction x1 y1 z1) + (Direction x2 y2 z2) = Direction (x1 + x2) (y1 + y2) (z1 + z2)
  --  (-) :: Direction -> Direction -> Direction
 {-# INLINE (-) #-}
 (Direction x1 y1 z1) - (Direction x2 y2 z2) = Direction (x1 - x2) (y1 - y2) (z1 - z2)

 -- Producto vectorial
--  (*) :: Direction -> Direction -> Direction
 {-# INLINE (*) #-}
 (Direction xa ya za) * (Direction xb yb zb) = Direction x y z
     where
        x = ya * zb - za * yb
        y = za * xb - xa * zb
        z = xa * yb - ya * xb

-- -- -- Producto escalar
{-# INLINE (.*) #-}
(.*) :: Direction -> Direction -> Float
(Direction xb yb zb) .* (Direction xa ya za)  = (xb*xa + yb*ya + zb*za)

-- -- -- Escalado de dirección
{-# INLINE escalateDir #-}
escalateDir :: Float -> Direction -> Direction
escalateDir s (Direction xa ya za) = Direction (s*xa) (s*ya) (s*za)

-- -- -- Escalado de puntos
{-# INLINE escalatePoint #-}
escalatePoint :: Float -> Point3D -> Point3D
escalatePoint s (Point3D xa ya za) = Point3D (s*xa) (s*ya) (s*za)

-- -- -- Escalado de dirección
{-# INLINE escalateDir' #-}
escalateDir' :: Float -> Direction -> Direction
escalateDir' s (Direction xa ya za) = Direction (s/xa) (s/ya) (s/za)

-- -- -- Escalado de puntos
{-# INLINE escalatePoint' #-}
escalatePoint' :: Float -> Point3D -> Point3D
escalatePoint' s (Point3D xa ya za) = Point3D (s/xa) (s/ya) (s/za)

-- -- Modulo
{-# INLINE modd #-}
modd :: Direction -> Float
modd (Direction x y z) = sqrt((x*x + y*y + z*z))

-- Normalización
{-# INLINE normal #-}
normal :: Direction -> Direction
normal  d@(Direction x y z) = let invLen = 1.0 / sqrt (x*x + y*y + z*z) in Direction (x*invLen) (y*invLen) (z*invLen)

-- Vector Perpendicular a otro
{-# INLINE perp #-}
perp :: Direction -> Direction
perp (Direction xb yb zb) = (Direction yb (-xb) zb)

--Generar Base con 3 Direcciones dadas(No comprueba que sean perpendiculares)
generateBase :: Direction -> Direction -> Direction -> Base
generateBase d0 d1 d2 = Base d0 d1 d2

{-# INLINE elevateRGBPoint #-}
elevateRGBPoint :: Float -> RGB -> RGB
elevateRGBPoint x (RGB r g b) =
    RGB (r ** (1.0 / x))
        (g ** (1.0 / x))
        (b ** (1.0 / x))

{-# INLINE sumRGBPoints #-}
sumRGBPoints :: RGB -> RGB -> RGB
sumRGBPoints (RGB r' g' b') (RGB r g b) =
    RGB (r + r')
        (g + g')
        (b + b')

{-# INLINE prodRGB #-}
prodRGB :: Float -> RGB -> RGB -> RGB
prodRGB f (RGB r0 g0 b0) (RGB r g b) =
    RGB (r * f * (r0/255))
        (g * f * (g0/255))
        (b * f * (b0/255))

{-# INLINE comparateRGB #-}
comparateRGB :: RGB -> RGB -> Bool
comparateRGB (RGB r' g' b') (RGB r g b) = (r == r') && (g == g') && (b == b')


-- --Punto a Vector
-- pointToVector :: Point3D -> Vector R
-- pointToVector (Point3D x y z) = fromList [realToFrac x, realToFrac y, realToFrac z, 1]

-- --Vector a Punto
-- vectorToPoint :: Vector R -> Point3D
-- vectorToPoint v = Point3D (realToFrac $ v ! 0) (realToFrac $ v ! 1) (realToFrac $ v ! 2)

-- --Base + Punto a Matriz
-- basePointMatrix :: Base -> Point3D -> Matrix R
-- basePointMatrix (Base (Direction a0 b0 c0) (Direction a1 b1 c1) (Direction a2 b2 c2)) (Point3D p1 p2 p3) =
--   (4><4) [realToFrac a0, realToFrac a1, realToFrac a2, realToFrac p1,
--           realToFrac b0, realToFrac b1, realToFrac b2, realToFrac p2,
--           realToFrac c0, realToFrac c1, realToFrac c2, realToFrac p3,
--           0.0, 0.0, 0.0, 1.0]

-- --Cambio de Base con punto y matriz en Global, devuelve punto visto en local
-- cambioBase :: Point3D -> Base -> Point3D -> Point3D
-- cambioBase nuevoOrigen baseACambiar puntoACambiarDeBase = puntoDeBaseCambiada
--     where
--         puntoDeBaseCambiada = roundTo5(vectorToPoint (baseNueva #> (pointToVector puntoACambiarDeBase)))
--         baseNueva = basePointMatrix baseACambiar nuevoOrigen

-- --Cambio de Base con punto y matriz en Local, devuelve punto visto en lglobal
-- cambioBase' :: Point3D -> Base -> Point3D -> Point3D
-- cambioBase' nuevoOrigen baseACambiar puntoACambiarDeBase = puntoDeBaseCambiada
--     where
--         puntoDeBaseCambiada = roundTo5(vectorToPoint vectorDeBaseCambiada)
--         vectorDeBaseCambiada = baseNueva #> (pointToVector puntoACambiarDeBase)
--         baseNueva =inv (basePointMatrix baseACambiar nuevoOrigen)

-- fullPrMtx :: Matrix R -> Matrix R
