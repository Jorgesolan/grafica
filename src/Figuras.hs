module Figuras where
import Elem3D

data Esfera = Esfera Point3D Float RGB
data Plano = Plano Point3D Direction RGB

parametricSphereCollision :: Esfera -> Ray -> (Float,RGB)
parametricSphereCollision (Esfera p0 r color) (Ray p1 d m) -- ya preguntaremos
  | raiz >= 0 = (findMinPositive(t0, t1), color)
  | otherwise = ((1/0),color)
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

parametricPlaneCollision :: Plano -> Ray -> (Float, RGB)
parametricPlaneCollision (Plano p0 n color) (Ray l0 d m) = ((((p0 #< l0) .* n) / (d .* n)),color)