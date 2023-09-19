module Base where
import System.IO ()

import Graphics.Gloss.Geometry.Angle
type Point3D = (Float, Float, Float)
type Direction = (Point3D)
type Base = (Direction, Direction, Direction)


rotatePoint :: Char -> Float -> Point3D -> Point3D
rotatePoint axis degree (x, y, z) 
 | axis == 'X' = (x, c*y + s*z, -s*y + c*z)
 | axis == 'Y' = (-s*z + c*x, y, s*x + c*z)
 | axis == 'Z' = (c*x + s*y, -s*x + c*y, z)
 | otherwise = (0,0,0)
    where radiant = normalizeAngle $ degToRad degree
          c = cos radiant
          s = sin radiant

-- rotateX :: Float -> Point3D -> Point3D

move :: Point3D -> Point3D -> Point3D
move (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian theta gamma radius = (x, y, z)
    where thetaRad = degToRad theta
          gammaRad = degToRad gamma
          sinTheta = sin thetaRad
          x = radius * sinTheta * cos gammaRad
          y = radius * sinTheta * sin gammaRad
          z = radius * cos thetaRad

-- instance Show Direction where
--     show (x y z) = "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ", D]"

-- instance Show Point3D where
--     show (x y z) = "[" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ " , P]"
    
-- -- Resta de puntos -> Dirección del primero al segundo
-- (#) :: Point3D -> Point3D -> Direction
-- xb yb zb # xa ya za  = Direction (xb-xa) (yb-ya) (zb-za)

-- -- desplazamiento de un punto
-- desplazarPunto :: Point3D -> Direction -> Direction
-- desplazarPunto (xb yb zb) (xa ya za)  = Direction (xb+xa) (yb+ya) (zb+za)

-- instance Num Direction where
-- -- Suma de direcciones
--  (+) :: Direction -> Direction -> Direction
--  (x1 y1 z1) + (x2 y2 z2) = Direction (x1 + x2) (y1 + y2) (z1 + z2)
--  -- Producto vectorial
--  (*) :: Direction -> Direction -> Direction
--  xa ya za * xb yb zb = Direction (ya*zb-za*yb) (za*xb-xa*zb) (xa*yb-ya*xb)


-- -- Producto escalar
-- (*.) :: Direction -> Direction -> Float
-- xb yb zb *. xa ya za  = xb*xa + yb*ya + zb*za

-- -- Escalado de dirección
-- escalarDir :: Float -> Direction -> Direction
-- escalarDir s (xa ya za) = Direction (s*xa) (s*ya) (s*za)

-- -- Modulo
-- modd :: Direction -> Float
-- modd (xb yb zb) = sqrt(xb**2 + yb**2 + zb**2)

-- -- Normalización
-- normal :: Direction -> Direction
-- normal (xb yb zb)
--     | bmod == 0 = Direction 0 0 0
--     | otherwise = Direction (xb / bmod) (yb / bmod) (zb / bmod)
--   where
--     bmod = modd (xb yb zb)
 
-- generateBase :: Base
-- generateBase = Base (Direction 1 0 0) (Direction 0 1 0) (Direction 0 0 1)

-- a = Point3D 3 3 3
-- b = Point3D 5 5 5
-- res = b # a

-- c = Direction 1 1 1
-- d = Direction 3 3 3
-- res2 = c + d
-- res4 = c *. d

-- res5 :: Direction
-- res5 = desplazarPunto a c
-- main :: IO ()
-- main = do
--     print res
--     print res2
--     print res4
--     print res5