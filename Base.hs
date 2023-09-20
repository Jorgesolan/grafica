module Base where

import System.IO ()
import Graphics.Gloss.Geometry.Angle

type Point = (Float, Float)
type Point3D = (Float, Float, Float)
type Direction = (Float, Float, Float)
type Base = (Direction, Direction, Direction)

floatTupleToIntTuple :: (Float, Float) -> (Int, Int)
floatTupleToIntTuple (x, y) = (round x, round y)

combinate2Tuples :: [Float] -> [Float] -> [(Float, Float)]
combinate2Tuples list1 list2 = [(x, y) | x <- list1, y <- list2]
combinate3Tuples :: [Float] -> [Float] -> [Float] -> [(Float, Float, Float)]
combinate3Tuples list1 list2 list3 = [(x, y, z) | x <- list1, y <- list2, z <- list3]

floatListToFloatTuple :: [Float] -> (Float, Float)
floatListToFloatTuple [x, y] = (x, y)
floatListToFloatTuple [_] = (0,0)
floatListToFloatTuple [] = (0,0)
floatListToFloatTuple (_:_:_:_) = (0,0)

-- showPoints3D :: [Point3D] -> IO ()
-- showPoints3D (h:s) = showPoint3D h $ showPoints3D s
-- showPoints3D [] = putStrLn $ ""

showPoint3D :: Point3D -> IO ()
showPoint3D (x, y, z) = putStrLn $ "Point3D: (" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

showDirection :: Direction -> IO ()
showDirection (x, y, z) = putStrLn $ "Direction: (" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"


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

project :: Float -> Float -> Float -> Point3D -> Point
project baseX baseY baseZ (x0, y0, z0) = (projectedX, projectedY)
    where (lookAtX, lookAtY, lookAtZ) = (0.0, 0.0, 0.0)
          (x, y, z) = (x0 - lookAtX, y0 - lookAtY, z0 - lookAtZ)
          (alpha, beta, gamma) = (degToRad 0.0, degToRad 0.0, degToRad 0.0)
          (cosAlpha, sinAlpha) = (cos alpha, sin alpha)
          (cosBeta, sinBeta) = (cos beta, sin beta)
          (cosGamma, sinGamma) = (cos gamma, sin gamma)
          (dx, dy, dz) = (cosBeta*(sinGamma*y + cosGamma*x) - sinBeta*z,
                          sinAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) +
                          cosAlpha*(cosGamma*y - sinGamma*x),
                          cosAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) -
                          sinAlpha*(cosGamma*y - sinGamma*x)) 
          projectedX = baseZ/dz*dx - baseX
          projectedY = baseZ/dz*dy - baseY
    
-- -- Resta de puntos -> Dirección del primero al segundo
(#) :: Point3D -> Point3D -> Direction
(xb, yb, zb) # (xa, ya, za)  = (xb-xa, yb-ya, zb-za)

-- instance Num Direction where
-- -- -- Suma de direcciones
(+++) :: Direction -> Direction -> Direction
(x1, y1, z1) +++ (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
-- --  -- Producto vectorial
(***) :: Direction -> Direction -> Direction
(xa, ya, za) *** (xb, yb, zb) = (ya*zb-za*yb, za*xb-xa*zb, xa*yb-ya*xb)


-- -- -- Producto escalar
(.*) :: Direction -> Direction -> Float
(xb, yb, zb) .* (xa, ya, za)  = (xb*xa + yb*ya + zb*za)

-- -- -- Escalado de dirección
escalateDir :: Float -> Direction -> Direction
escalateDir s (xa, ya, za) = (s*xa, s*ya, s*za)

-- -- -- Escalado de puntos
escalatePoint :: Float -> Point3D -> Point3D
escalatePoint s (xa, ya, za) = (s*xa, s*ya, s*za)


-- -- Modulo
modd :: Direction -> Float
modd (xb, yb, zb) = sqrt(xb**2 + yb**2 + zb**2)

-- Normalización
normal :: Direction -> Direction
normal (xb, yb, zb)
     | bmod == 0 = (0, 0, 0)
     | otherwise = ((xb / bmod), (yb / bmod), (zb / bmod))
   where
     bmod = modd (xb, yb, zb)
 
generateBase :: Base
generateBase = ((1, 0, 0), (0, 1, 0), (0, 0, 1))