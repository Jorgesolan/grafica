{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
module Elem3D where
import Data.Binary ( Binary(get, put) )
import System.IO ()
import Data.List (transpose)
import Debug.Trace (trace)
import qualified Data.Binary.Put
import qualified Data.Binary as Data.Binary.Get.Internal

data Point3D = Point3D {xP :: !Float, yP :: !Float, zP :: !Float} deriving (Eq)
data Direction = Direction {xD :: !Float, yD :: !Float, zD :: !Float} deriving (Eq)
data Ray = Ray {oR :: !Point3D, dR :: !Direction}
data Base = Base !Direction !Direction !Direction
data RGB = RGB {red :: !Float, green :: !Float, blue :: !Float} deriving (Eq)
data Luz = Luz !Point3D !RGB !Float
data Foton = Foton {pFot :: !Point3D, iFot :: Float, rgbFot :: RGB, idFot :: Int}

instance Binary Point3D where
  put :: Point3D -> Data.Binary.Put.Put
  put (Point3D {..}) = put xP >> put yP >> put zP
  get :: Data.Binary.Get.Internal.Get Point3D
  get = do
    x <- get
    y <- get
    Point3D x y <$> get

instance Binary Direction where
  put :: Direction -> Data.Binary.Get.Internal.Put
  put (Direction {..}) = put xD >> put yD >> put zD
  get :: Data.Binary.Get.Internal.Get Direction
  get = do
    x <- get
    y <- get
    Direction x y <$> get

instance Binary RGB where
  put :: RGB -> Data.Binary.Get.Internal.Put
  put (RGB {..}) = put red >> put green >> put blue
  get :: Data.Binary.Get.Internal.Get RGB
  get = do
    r <- get
    g <- get
    RGB r g <$> get

instance Binary Foton where
  put :: Foton -> Data.Binary.Get.Internal.Put
  put (Foton {..}) = put pFot >> put iFot  >> put rgbFot >> put idFot
  get :: Data.Binary.Get.Internal.Get Foton
  get = do
    p <- get
    f <- get
    c <- get
    Foton p f c <$> get

instance Show Base where
    show :: Base -> String
    show (Base (Direction dx0 dy0 dz0) (Direction dx1 dy1 dz1) (Direction dx2 dy2 dz2)) = "Base:\n| " ++ show dx0 ++" " ++show dy0 ++" "++ show dz0 ++ " |\n" ++ "| " ++ show dx1 ++" " ++show dy1 ++" "++ show dz1 ++ " |\n"++ "| " ++ show dx2 ++" " ++show dy2 ++" "++ show dz2 ++ "| "

instance Show Point3D where
    show :: Point3D -> String
    show (Point3D{..}) = "Point3D " ++ show xP ++ " " ++ show yP ++ " " ++ show zP

instance Show Direction where
    show :: Direction -> String
    show (Direction {..}) = "Direction " ++ show xD ++ " " ++ show yD ++ " " ++ show zD

instance Show Ray where
    show :: Ray -> String
    show (Ray p d) = "Rayo hasta "  ++ show d

instance Show RGB where
    show :: RGB -> String
    show (RGB {..}) = "R "  ++ show red ++ " G "  ++ show green ++ " B "  ++ show blue

instance Show Foton where
    show :: Foton -> String
    show (Foton (Point3D x y z) i j  _) = "Foton " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show i ++ " " ++ show j

getX :: Point3D -> Float
getX (Point3D x _ _) = x

getY :: Point3D -> Float
getY (Point3D _ y _) = y

getZ :: Point3D -> Float
getZ (Point3D _ _ z) = z

{-# INLINE getPhotonID #-}
getPhotonID :: Foton -> Int
getPhotonID (Foton {..}) = idFot

{-# INLINE obtenerRayo #-}
obtenerRayo :: Ray -> Direction
obtenerRayo (Ray _ dir) = dir

{-# INLINE roundTo #-}
roundTo :: Int -> Point3D -> Point3D
roundTo n (Point3D a b c) = Point3D a' b' c'
    where
        !a' = fromInteger $ round $ a * (10^n) / (10.0^^n) :: Float
        !b' = fromInteger $ round $ b * (10^n) / (10.0^^n) :: Float
        !c' = fromInteger $ round $ c * (10^n) / (10.0^^n) :: Float

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

-----------------------------------------------------------------------------------------------------------------------------------------------
--Puntos

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

rotatePointt :: Char -> Float -> Point3D -> Point3D
rotatePointt axis radiant  (Point3D x y z)
 | radiant == 0 = Point3D x y z
 | axis == 'X' = Point3D x (c*y + s*z) (-s*y + c*z)
 | axis == 'Y' = Point3D (-s*z + c*x) y (s*x + c*z)
 | axis == 'Z' = Point3D (c*x + s*y) (-s*x + c*y) z
 | otherwise = Point3D 0 0 0
    where
        !c = cos $ degToRad radiant
        !s = sin $ degToRad radiant

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
movePoint (Direction {..}) (Point3D {..}) = Point3D (xD + xP) (yD + yP) (zD + zP)

{-# INLINE movePoint' #-}
movePoint' :: Direction -> Point3D -> Point3D
movePoint' (Direction {..}) (Point3D {..}) = Point3D (xD - xP) (yD - yP) (zD - zP)

{-# INLINE distPoint #-}
distPoint :: Point3D -> Point3D -> Float
distPoint (Point3D x1 y1 z1) (Point3D x2 y2 z2) = sqrt $ (x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2

-- -- Resta de puntos -> Dirección del primero al segundo
{-# INLINE (#) #-}
(#) :: Point3D -> Point3D -> Point3D
(Point3D !xb !yb !zb) # (Point3D !xa !ya !za) = Point3D (xb-xa) (yb-ya) (zb-za)

{-# INLINE addPoints #-}
addPoints :: Point3D -> Point3D -> Point3D
addPoints (Point3D !xb !yb !zb) (Point3D !xa !ya !za) = Point3D (xb+xa) (yb+ya) (zb+za)


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

-- -- -- Escalado de puntos
{-# INLINE escalatePoint' #-}
escalatePoint' :: Float -> Point3D -> Point3D
escalatePoint' s (Point3D {..}) = Point3D (xP/s) (yP/s) (zP/s)

-- -- -- Escalado de puntos
{-# INLINE escalatePoint #-}
escalatePoint :: Point3D -> Float -> Point3D
escalatePoint (Point3D {..}) s = Point3D (s*xP) (s*yP) (s*zP)
{-# INLINE escalatePointt #-}
escalatePointt :: Float -> Point3D -> Point3D
escalatePointt s (Point3D {..}) = Point3D (s*xP) (s*yP) (s*zP)

{-# INLINE pointDir#-}
pointDir :: Point3D -> Direction
pointDir (Point3D x y z) = Direction x y z

{-# INLINE dirPoint#-}
dirPoint :: Direction -> Point3D
dirPoint (Direction x y z) = Point3D x y z

{-# INLINE pointToPothon#-}
pointToPothon :: Point3D -> Foton
pointToPothon p = Foton p 0 (RGB 0 0 0) 0

{-# INLINE distFot #-}
distFot :: Point3D -> Foton -> Float
distFot p fot = abs $ p `distPoint` pFot fot

------------------------------------------------------------------------------------------------------------------
--Direcciones

instance Num Direction where
-- Suma de direcciones
 {-# INLINE (+) #-}
 (+) :: Direction -> Direction -> Direction
 (Direction x1 y1 z1) + (Direction x2 y2 z2) = Direction (x1 + x2) (y1 + y2) (z1 + z2)
-- Resta de Direcciones
 {-# INLINE (-) #-}
 (-) :: Direction -> Direction -> Direction
 (Direction x1 y1 z1) - (Direction x2 y2 z2) = Direction (x1 - x2) (y1 - y2) (z1 - z2)

 -- Producto vectorial
 {-# INLINE (*) #-}
 (*) :: Direction -> Direction -> Direction
 dir0 * dir1 = Direction x y z
     where
        x = yD dir0 * zD dir1- zD dir0 * yD dir1
        y = zD dir0* xD dir1 - xD dir0* zD dir1
        z = xD dir0* yD dir1 - yD dir0 * xD dir1

-- -- -- Producto escalar
{-# INLINE (.*) #-}
(.*) :: Direction -> Direction -> Float
dir0 .* dir1  = xD dir0*xD dir1 + yD dir0*yD dir1 + zD dir0*zD dir1

-- -- -- Escalado de dirección
{-# INLINE escalateDir #-}
escalateDir :: Float -> Direction -> Direction
escalateDir s (Direction {..}) = Direction (s*xD) (s*yD) (s*zD)

-- -- -- Escalado de dirección
{-# INLINE escalateDir' #-}
escalateDir' :: Float -> Direction -> Direction
escalateDir' s (Direction {..}) = Direction (xD/s) (yD/s) (zD/s)

-- -- Modulo
{-# INLINE modd #-}
modd :: Direction -> Float
modd (Direction {..}) = sqrt (xD*xD + yD*yD + zD*zD)

-- Normalización
{-# INLINE normal #-}
normal :: Direction -> Direction
normal (Direction {..}) = let !invLen = 1.0 / sqrt (xD*xD + yD*yD + zD*zD) in Direction (xD*invLen) (yD*invLen) (zD*invLen)

-----------------------------------------------------------------------------------------------------------------------------------------------
--RGB

{-# INLINE elevateRGBPoint #-}
elevateRGBPoint :: Float -> RGB -> RGB
elevateRGBPoint x (RGB {..}) =
    RGB (red ** (1.0 / x))
        (green ** (1.0 / x))
        (blue ** (1.0 / x))

instance Num RGB where

 {-# INLINE (+) #-}
 (+) :: RGB -> RGB -> RGB
 (RGB r1 g1 b1) + (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

 {-# INLINE (-) #-}
 (-) :: RGB -> RGB -> RGB
 (RGB r1 g1 b1) - (RGB r2 g2 b2) = RGB (r1 - r2) (g1 - g2) (b1 - b2)

 {-# INLINE (*) #-}
 (*) :: RGB -> RGB -> RGB
 (RGB r1 g1 b1) * (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)

{-# INLINE (./) #-}
(./) :: RGB -> RGB -> RGB
(RGB r1 g1 b1) ./ (RGB r2 g2 b2) = RGB (r1 / r2) (g1 / g2) (b1 / b2)

{-# INLINE modRGB #-}
modRGB :: RGB -> Float -> RGB
modRGB (RGB {..}) f =
    RGB (red * f)
        (green * f)
        (blue * f)

{-# INLINE divRGB #-}
divRGB :: RGB -> Float -> RGB
divRGB (RGB {..}) f =
    RGB (red / f)
        (green / f)
        (blue / f)
{-# INLINE scale #-}
scale :: RGB -> RGB
scale x = x ./ RGB 255 255 255

{-# INLINE prodRGB #-}
prodRGB ::  RGB -> RGB -> Float -> RGB
prodRGB r0 r1 = modRGB (scale r0 * r1)

{-# INLINE nanRGB #-}
nanRGB :: RGB -> Bool
nanRGB (RGB r g b) = isNaN r || isNaN g || isNaN b


-----------------------------------------------------------------------------------------------------------------------------------------------
--Bases y Matrices


-- Punto a Vector
pointToVector :: Point3D -> [Float]
pointToVector (Point3D x y z) = [x, y, z]

-- Vector a Punto
vectorToPoint :: [Float] -> Point3D
vectorToPoint [x, y, z] = Point3D x y z

--Generar Base con 3 Direcciones dadas(No comprueba que sean perpendiculares)
{-# INLINE generateBase #-}
generateBase :: Direction -> Direction -> Direction -> Base
generateBase = Base

-- Base + Punto a Matriz
basePointMatrix :: Base -> [[Float]]
basePointMatrix (Base (Direction a0 b0 c0) (Direction a1 b1 c1) (Direction a2 b2 c2)) =
  [[a0, a1, a2],
   [b0, b1, b2],
   [c0, c1, c2]]

--Cambio de Base con punto y matriz en Global, devuelve punto visto en local
cambioBase :: Point3D -> Base -> Point3D -> Point3D
cambioBase nuevoOrigen baseACambiar puntoACambiarDeBase = movePoint (nuevoOrigen #< Point3D 0 0 0) puntoDeBaseCambiada
    where
        puntoDeBaseCambiada = vectorToPoint vectorDeBaseCambiada
        vectorDeBaseCambiada = matrixVectorProduct baseNueva (pointToVector puntoACambiarDeBase)
        baseNueva = basePointMatrix baseACambiar

--Cambio de Base con punto y matriz en Local, devuelve punto visto en global
cambioBase' :: Point3D -> Base -> Point3D -> Point3D
cambioBase' nuevoOrigen baseACambiar puntoACambiarDeBase = movePoint (nuevoOrigen #< Point3D 0 0 0) puntoDeBaseCambiada
    where
        puntoDeBaseCambiada = roundTo 5 $ vectorToPoint vectorDeBaseCambiada
        vectorDeBaseCambiada = matrixVectorProduct (transposeMatrix baseNueva) (pointToVector puntoACambiarDeBase)
        baseNueva = invertMatrix (basePointMatrix baseACambiar)

-- Producto de matriz por vector
matrixVectorProduct :: [[Float]] -> [Float] -> [Float]
matrixVectorProduct mat vec =
  [sum $ zipWith (*) row vec | row <- mat]

-- Función de transposición de una matriz
transposeMatrix :: [[a]] -> [[a]]
transposeMatrix ([]:_) = []
transposeMatrix x = map head x : transposeMatrix (map tail x)

invertMatrix :: [[Float]] -> [[Float]]
invertMatrix [[a, b, c], [d, e, f], [g, h, i]] =
  let det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
  in [[(e * i - f * h) / det, (c * h - b * i) / det, (b * f - c * e) / det],
      [(f * g - d * i) / det, (a * i - c * g) / det, (c * d - a * f) / det],
      [(d * h - e * g) / det, (g * b - a * h) / det, (a * e - b * d) / det]]