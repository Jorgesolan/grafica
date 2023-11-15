{-# LANGUAGE BangPatterns #-}
module Elem3D where

import System.IO ()
import Numeric.LinearAlgebra (Vector, Matrix, fromList, inv, (!), (><), R, (#>))

data Point3D = Point3D !Float !Float !Float
data Direction = Direction !Float !Float !Float
data Ray = Ray !Point3D !Direction
data Base = Base !Direction !Direction !Direction
data RGB = RGB !Float !Float !Float
data Luz = Luz !Point3D !RGB !Float
data Foton = Foton !Point3D Float Float

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
    show (Ray p d) = "Rayo hasta "  ++ show d

instance Show RGB where
    show (RGB r g b) = "R "  ++ show r ++ " G "  ++ show g ++ " B "  ++ show b

{-# INLINE obtenerRayo #-}
obtenerRayo :: Ray -> Direction
obtenerRayo (Ray _ dir) = dir

{-# INLINE roundTo #-}
roundTo :: Int -> Point3D -> Point3D
roundTo n (Point3D a b c) = Point3D a' b' c'
    where
        !a' = (fromInteger $ round $ a * (10^n)) / (10.0^^n) :: Float
        !b' = (fromInteger $ round $ b * (10^n)) / (10.0^^n) :: Float
        !c' = (fromInteger $ round $ c * (10^n)) / (10.0^^n) :: Float

-- ************************************************
-- Angulos y transformaciones
{-# INLINE radToDeg #-}
radToDeg :: Float -> Float
radToDeg radians = radians * (180.0 / pi)

{-# INLINE degToRad #-}
degToRad :: Float -> Float
degToRad degree = degree * (pi / 180.0)

{-# INLINE angleBetween #-}
angleBetween :: Direction -> Direction -> Float
angleBetween d1 d2 = acos $ d1 .* d2 / (modd d1 * modd d2)

-- ************************************************

rotatePoint :: Char -> Float -> Point3D -> Point3D
rotatePoint axis radiant  (Point3D x y z)
 | radiant == 0 = Point3D x y z
 | axis == 'X' = Point3D x (c*y + s*z) (-s*y + c*z)
 | axis == 'Y' = Point3D (-s*z + c*x) y (s*x + c*z)
 | axis == 'Z' = Point3D (c*x + s*y) (-s*x + c*y) z
 | otherwise = Point3D 0 0 0
    where
        !c = cos radiant
        !s = sin radiant

rotatePoint' :: Char -> Float -> Point3D -> Point3D
rotatePoint' axis radiant  (Point3D x y z)
 | radiant == 0 = Point3D x y z
 | axis == 'X' = Point3D x (c*y + s*z) (-s*y + c*z)
 | axis == 'Y' = Point3D (-s*z + c*x) y (s*x + c*z)
 | axis == 'Z' = Point3D (c*x + s*y) (-s*x + c*y) z
 | otherwise = Point3D 0 0 0
    where 
        radiant' = 2*pi - radiant
        c = cos radiant
        s = sin radiant

{-# INLINE movePoint #-}
movePoint :: Direction -> Point3D -> Point3D
movePoint (Direction !dx !dy !dz) (Point3D !x !y !z) = Point3D (x + dx) (y + dy) (z + dz)

{-# INLINE movePoint' #-}
movePoint' :: Direction -> Point3D -> Point3D
movePoint' (Direction dx dy dz) (Point3D x y z) = Point3D (x - dx) (y - dy) (z - dz)

{-# INLINE distPoint #-}
distPoint :: Point3D -> Point3D -> Float
distPoint (Point3D x1 y1 z1) (Point3D x2 y2 z2) = sqrt $ (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2

-- -- Resta de puntos -> Dirección del primero al segundo
{-# INLINE (#) #-}
(#) :: Point3D -> Point3D -> Point3D
(Point3D !xb !yb !zb) # (Point3D !xa !ya !za) = Point3D (xb-xa) (yb-ya) (zb-za)

-- -- Direccion entre puntos -> Dirección del primero al segundo
{-# INLINE (#<) #-}
(#<) :: Point3D -> Point3D -> Direction
(Point3D !xb !yb !zb) #< (Point3D !xa !ya !za) = Direction (xb-xa) (yb-ya) (zb-za)

{-# INLINE aproxPoint #-}
aproxPoint :: Point3D -> Point3D -> Bool
aproxPoint (Point3D !xb !yb zb) (Point3D !xa !ya !za) = a && b && c
    where
        !a = abs (xb-xa) < 0.1
        !b = abs (yb-ya) < 0.1
        !c = abs (zb-za) < 0.1

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
(Direction xb yb zb) .* (Direction xa ya za)  = xb*xa + yb*ya + zb*za

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
modd (Direction !x !y !z) = sqrt(x*x + y*y + z*z)

-- Normalización
{-# INLINE normal #-}
normal :: Direction -> Direction
normal (Direction !x !y !z) = let !invLen = 1.0 / sqrt (x*x + y*y + z*z) in Direction (x*invLen) (y*invLen) (z*invLen)

--Generar Base con 3 Direcciones dadas(No comprueba que sean perpendiculares)
{-# INLINE generateBase #-}
generateBase :: Direction -> Direction -> Direction -> Base
generateBase d0 d1 d2 = Base d0 d1 d2

{-# INLINE elevateRGBPoint #-}
elevateRGBPoint :: Float -> RGB -> RGB
elevateRGBPoint x (RGB r g b) =
    RGB (r ** (1.0 / x))
        (g ** (1.0 / x))
        (b ** (1.0 / x))

instance Num RGB where

 {-# INLINE (+) #-}
 (RGB r1 g1 b1) + (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

 {-# INLINE (-) #-}
 (RGB r1 g1 b1) - (RGB r2 g2 b2) = RGB (r1 - r2) (g1 - g2) (b1 - b2)

 {-# INLINE (*) #-}
 (RGB r1 g1 b1) * (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)

{-# INLINE (./) #-} 
(./) :: RGB -> RGB -> RGB
(RGB r1 g1 b1) ./ (RGB r2 g2 b2) = RGB (r1 / r2) (g1 / g2) (b1 / b2)

{-# INLINE modRGB #-}
modRGB :: RGB -> Float -> RGB
modRGB (RGB r g b) f =
    RGB (r * f)
        (g * f)
        (b * f)

{-# INLINE divRGB #-}
divRGB :: RGB -> Float -> RGB
divRGB (RGB r g b) f =
    RGB (r / f)
        (g / f)
        (b / f)
{-# INLINE scale #-}
scale :: RGB -> RGB
scale x = x ./ (RGB 255 255 255)

{-# INLINE prodRGB #-}
prodRGB ::  RGB -> RGB -> Float -> RGB
prodRGB r0 r1 f = modRGB ((scale r0) * r1) f 

{-# INLINE comparateRGB #-}
comparateRGB :: RGB -> RGB -> Bool
comparateRGB (RGB r' g' b') (RGB r g b) = r == r' && g == g' && b == b'


--Punto a Vector
pointToVector :: Point3D -> Vector R
pointToVector (Point3D x y z) = fromList [realToFrac x, realToFrac y, realToFrac z, 1]


--Vector a Punto
vectorToPoint :: Vector R -> Point3D
vectorToPoint v = Point3D (realToFrac $ v ! 0) (realToFrac $ v ! 1) (realToFrac $ v ! 2)

--Base + Punto a Matriz
basePointMatrix :: Base -> Matrix R
basePointMatrix (Base (Direction a0 b0 c0) (Direction a1 b1 c1) (Direction a2 b2 c2)) =
  (3><3) [realToFrac a0, realToFrac a1, realToFrac a2,
          realToFrac b0, realToFrac b1, realToFrac b2,
          realToFrac c0, realToFrac c1, realToFrac c2]

--Cambio de Base con punto y matriz en Global, devuelve punto visto en local
cambioBase :: Point3D -> Base -> Point3D -> Point3D
cambioBase nuevoOrigen baseACambiar puntoACambiarDeBase = movePoint (nuevoOrigen #< Point3D 0 0 0) puntoDeBaseCambiada
    where
        puntoDeBaseCambiada = roundTo 5 $ vectorToPoint (baseNueva #> (pointToVector' puntoACambiarDeBase))
        baseNueva = basePointMatrix baseACambiar

--Cambio de Base con punto y matriz en Local, devuelve punto visto en global
cambioBase' :: Point3D -> Base -> Point3D -> Point3D
cambioBase' nuevoOrigen baseACambiar puntoACambiarDeBase = movePoint (nuevoOrigen #< Point3D 0 0 0) puntoDeBaseCambiada
    where
        puntoDeBaseCambiada = roundTo 5 $ vectorToPoint vectorDeBaseCambiada
        vectorDeBaseCambiada = baseNueva #> (pointToVector' puntoACambiarDeBase)
        baseNueva = inv (basePointMatrix baseACambiar)

--Punto a Vector
pointToVector' :: Point3D -> Vector R
pointToVector' (Point3D x y z) = fromList [realToFrac x, realToFrac y, realToFrac z]

-- fullPrMtx :: Matrix R -> Matrix R
