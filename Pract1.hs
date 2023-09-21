import Elem3D
import System.Random
import Graphics.Gloss.Geometry.Angle

polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian theta gamma radius = (Point3D x y z)
    where thetaRad = degToRad theta
          gammaRad = degToRad gamma
          sinTheta = sin thetaRad
          x = radius * sinTheta * cos gammaRad
          y = radius * sinTheta * sin gammaRad
          z = radius * cos thetaRad

ciudad :: Float -> Direction -> Point3D
ciudad rad d = movePoint d (polarToCartesian t g rad )
      where   
            t = head (take 1 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42)
            g = head (take 1 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337)

e0 = Point3D 0 0 0 --Origen
e1 = Point3D 100 0 10 -- Eje del primer planeta
e2 = Point3D (-200) 500 100 --Eje del segundo planeta

p1 = roundTo5(ciudad  200 (e1 #< e0))
p2 = roundTo5(ciudad 10 (e2 #< e0))

y1 = normal (p1 #< e1)
x1 = perp y1
z1 = y1 * x1
d1 = generateBase x1 y1 z1

y2 = normal (p2 #< e2)
x2 = perp y2
z2 = y2 * x2
d2 = generateBase x2 y2 z2

algo = cambioBase p1 d1 p2
a = Point3D 2 0 1 


main :: IO ()
main = do
    print a
    print a' 
    print a''
    print p1
    print p2
    print d1
    print algo
    where
        nuevaBase = (Base (Direction 1 2 3)  (Direction (-1) 0 1) (Direction 2 (-3) (1)))
        nuevoOrigen = (Point3D 1 2 0) 
        a' = cambioBase nuevoOrigen nuevaBase a
        a'' = cambioBase' nuevoOrigen nuevaBase a'
