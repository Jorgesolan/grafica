module Base where

import System.IO ()
import Graphics.Gloss.Geometry.Angle


type Point3D = (Float, Float, Float)
type Direction = (Float, Float, Float)
type Base = (Direction, Direction, Direction)


showPoint3D :: Point3D -> String
showPoint3D (x, y, z) = "Point3D: (" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

showDirection :: Direction -> String
showDirection (x, y, z) = "Direction: (" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"


rotatePoint :: Char -> Float -> Point3D -> Point3D
rotatePoint axis degree (x, y, z) 
 | axis == 'X' = (x, c*y + s*z, -s*y + c*z)
 | axis == 'Y' = (-s*z + c*x, y, s*x + c*z)
 | axis == 'Z' = (c*x + s*y, -s*x + c*y, z)
 | otherwise = (0,0,0)
    where radiant = normalizeAngle $ degToRad degree
          c = cos radiant
          s = sin radiant


movePoint :: Direction -> Point3D -> Point3D
movePoint (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian theta gamma radius = (x, y, z)
    where thetaRad = degToRad theta
          gammaRad = degToRad gamma
          sinTheta = sin thetaRad
          x = radius * sinTheta * cos gammaRad
          y = radius * sinTheta * sin gammaRad
          z = radius * cos thetaRad

    
-- -- Resta de puntos -> Dirección del primero al segundo
(#) :: Point3D -> Point3D -> Direction
(xb, yb, zb) # (xa, ya, za)  = (xb-xa, yb-ya, zb-za)

-- instance Num Direction where
-- -- -- Suma de direcciones
concatDirections :: Direction -> Direction -> Direction
concatDirections (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
-- --  -- Producto vectorial
vectorialProd :: Direction -> Direction -> Direction
vectorialProd (xa, ya, za) (xb, yb, zb) = (ya*zb-za*yb, za*xb-xa*zb, xa*yb-ya*xb)


-- -- -- Producto escalar
escalarProd :: Direction -> Direction -> Float
escalarProd (xb, yb, zb) (xa, ya, za)  = (xb*xa + yb*ya + zb*za)

-- -- -- Escalado de dirección
escalateDir :: Float -> Direction -> Direction
escalateDir s (xa, ya, za) = (s*xa, s*ya, s*za)

-- -- -- Modulo
--  modd :: Direction -> Float
--  modd (xb, yb, zb) = sqrt(xb**2 + yb**2 + zb**2)

-- -- Normalización
--  normal :: Direction -> Direction
--  normal (xb, yb, zb)
--      | bmod == 0 = (0, 0, 0)
--      | otherwise = (xb / bmod), (yb / bmod), (zb / bmod)
--    where
--      bmod = modd (xb, yb, zb)
 
-- generateBase :: Base
-- generateBase = Base (Direction 1 0 0) (Direction 0 1 0) (Direction 0 0 1)