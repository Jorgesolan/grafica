module Figuras where
import Elem3D
import Debug.Trace

data Camara = Camara Point3D Base
data Esfera = Esfera Point3D Float RGB Float
data Plano = Plano Point3D Direction RGB Float
data Triangulo = Triangulo Point3D Point3D Point3D RGB
data Shape = Sphere Esfera | Plane Plano

parametricShapeCollision :: [Shape] -> [Ray] -> [[(Float, (RGB, Float, Point3D, Direction))]]
parametricShapeCollision shapes rays = map (collision rays) shapes
  where
    collision :: [Ray] -> Shape -> [(Float, (RGB, Float, Point3D, Direction))]
    collision rays shape = map (oneCollision shape) rays

    oneCollision :: Shape -> Ray -> (Float, (RGB, Float, Point3D, Direction))
    oneCollision (Sphere (Esfera p0 r color reflectividad)) (Ray p1 d m)
      | raiz >= 0 = (mind, (color,reflectividad, collisionPoint, normal)) 
      | otherwise = ((1/0),(color,reflectividad,Point3D (1/0) (1/0) (1/0),Direction (1/0) (1/0) (1/0)))     
        where
          f = p1 #< p0
          a = d .* d
          b = 2 * (f .* d)
          c = f .* f - r**2
          raiz = b**2 - 4*a*c
          resul = sqrt raiz
          t0 = (-b + resul) / (2*a)
          t1 = (-b - resul) / (2*a)
          findMinPositive :: (Float, Float) -> Float
          findMinPositive (x, y)
            | null positiveValues = (1/0)
            | otherwise           = minimum positiveValues
            where
              positiveValues = filter (> 0) [x, y]
          mind = findMinPositive (t0, t1)
          collisionPoint = movePoint (escalateDir mind d) p1
          normal = collisionPoint #< p0


    oneCollision (Plane (Plano p0 n color reflectividad)) (Ray p1 d m) = (mind,(color, reflectividad, collisionPoint, normal))
      where
        mind = (((p0 #< p1) .* n) / (d .* n))
        collisionPoint = movePoint (escalateDir mind d) p1
        normal = n * d

parametricTriangleCollision :: Triangulo -> Ray -> (Float, RGB)
parametricTriangleCollision (Triangulo v0 v1 v2 color) (Ray l0 d m) =
  if detA == 0.0
    then ((1/0),color)  -- Ray is parallel to the triangle, no collision
    else if t < 0.0
      then ((1/0),color)  -- Triangle is behind the ray's origin
      else let u = (uNum / detA)
               v = (vNum / detA)
           in if u >= 0.0 && v >= 0.0 && u + v <= 1.0
                then (t, color)
                else ((1/0),color)  -- Intersection point is outside the triangle
  where
    edge1 = v1 #< v0
    edge2 = v2 #< v0
    h = d * edge2
    a = edge1 .* h

    detA = edge1 .* h

    f = 1.0 / a
    s = l0 #< v0

    uNum = f * (s .* h)
    u = uNum / detA

    q = (s * edge1)
    vNum = f * (d .* q)
    v = vNum / detA

    tNum = f * (edge2 .* q)
    t = tNum / detA