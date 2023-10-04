module Figuras where
import Elem3D

data Esfera = Esfera { centro :: Point3D, radio :: Float}
parametricSphereCollision :: Esfera -> Ray -> Maybe(Float)
parametricSphereCollision (Esfera p0 r) (Ray p1 d m) -- ya preguntaremos
  | raiz >= 0 = Just (findMinPositive(t0, t1))
  | otherwise = Nothing
      where
            t0 = (-b+raiz)/(2*a)
            t1 = (-b-raiz)/(2*a)
            a = (d.*d)
            b = 2*(f.*d)
            c = (f.*f) - (r**2)
            raiz = sqrt((b**2) - (4*a*c))
            ori = Point3D 0 0 0
            f =  ori #< p0
            findMinPositive :: (Float, Float) -> Float
            findMinPositive (x, y) = minimum [a | a <- [x, y], a > 0]

