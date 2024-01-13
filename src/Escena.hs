module Escena where   

import Elem3D
    ( Foton,
      Luz(..),
      RGB(..),
      Base(Base),
      Ray,
      Direction(Direction),
      Point3D(..),
      escalatePoint,
      degToRad,
      rotatePoint,movePoint
    )

import Figuras
    ( Obj, Point2D(..),
      Shape(Sphere, Plane, Cylinder,Rectangle,Cone,Triangle),
      Plano(Plano),
      Esfera(Esfera),
      Cilindro(Cilindro),
      Rectangulo(Rectangulo),
      Camara(Camara),
      Cono(Cono),
      Triangulo(Triangulo),
      addFigMult,
      parametricShapeCollision,
      loadObjFile,
      convertToCustomFormat, encenderShape ,encenderShapes
      )
import qualified Data.DList as DL
import qualified Data.Set as Set


-- CARGAKD
intMxKd :: Float
intMxKd = 50
n :: Float
n=100000
nRebotes :: Int
nRebotes = 12
-- SIMULACION
aspectR :: Float
aspectR = 1
pix :: Float
pix = 24
piCam :: Float
piCam = 15
gamma :: Float
gamma = 2.4
maxN :: Int
maxN = 12
etapasY :: Int
etapasY = 1
etapasX:: Int
etapasX = 1
nRay :: Int
nRay = 1
intMxSim :: Float
intMxSim = 1.0

basCam :: Base
basCam = Base (Direction (piCam*aspectR) 0 0) (Direction 0 piCam 0) (Direction 0 0 (17.5))

cam :: Point3D
cam =  Point3D 0 0 35

-- ESCENA
centr :: Point3D
centr = Point3D (-9) (-8) (1)
-- centr' :: Point3D
centr' = Point3D (10) (-8) (2)
-- centr'' :: Point3D
-- centr'' = Point3D 0 (-15) (-44)
-- centr''' :: Point3D
-- centr''' = Point3D 15 (-10) (-22)

luz :: Luz
luz = Luz (Point3D (0) 8 (-3)) (RGB 255 255 255) 1 -- Dentro
luz' :: Luz
luz' = Luz (Point3D (10) (5) (-50)) (RGB 255 255 255) (intMxKd/2) -- Fuera de la escena
luz'' :: Luz
luz'' = Luz (Point3D 8 3 (-12))  (RGB 255 255 255) intMxKd -- Al lado de columna
luz''' :: Luz
luz''' = Luz (Point3D 0 0 (30))  (RGB 255 255 255) 1 -- Al lado de columna

plano0 :: Shape
plano0 = Plane (Plano (Point3D (-15) 0 0) (Direction 1 0 0) (RGB 255 10 10) (0.85,0,0) 0 0) --Izq
plano1 :: Shape
plano1 =  Plane (Plano (Point3D 15 0 0) (Direction 1 0 0) (RGB 10 255 10) (0.85,0,0) 0 0) -- Der
plano2 :: Shape
plano2 =  Plane (Plano (Point3D 0 15 0) (Direction 0 1 0) (RGB 100 100 100) (0.85,0,0) 0 0) -- Techo
plano3 :: Shape
plano3 =  Plane (Plano (Point3D 0 0 (-15)) (Direction 0 0 (-1)) (RGB 1 1 1) (0.85,0,0) 0 0) -- Fondo
-- plano4 :: Shape
plano4 =  Plane (Plano (Point3D 0 (-15) 0) (Direction 0 1 0) (RGB 100 100 100) (0.85,0,0) 1.6 0) -- Suelo
plano5 :: Shape
plano5 =  Plane (Plano (Point3D 0 0 50.5) (Direction 0 0 1) (RGB 1 1 1) (0.85,0,0) 0 0) -- Detras Camara
bola :: Shape
bola =  Sphere (Esfera centr 2 (RGB 250 10 208) (0.5, 0,0) 1.5 0)
bola' :: Shape
bola' =  Sphere (Esfera centr' 2 (RGB 10 250 200) (0.85,0,0) 1.5 0)
-- bola'' :: Shape
-- bola'' =  Sphere (Esfera centr'' 4 (RGB 150 0 0 ) (0.8, 0, 0) 0 0)
-- bola''' :: Shape
-- bola''' =  Sphere (Esfera centr''' 5 (RGB 122 210 155) (0.8,0,0) 1.5 0)
-- rect0 :: Shape
rect0 = Rectangle (Rectangulo (Point3D 15 0 (-10)) (Direction (1) 0 0) (Direction 0 0 (-1)) 30 30 (RGB 10 255 10) (0.85,0,0) 0 0) -- Der
-- rect1 :: Shape
rect1 = Rectangle (Rectangulo (Point3D (-15) 0 (-10)) (Direction 1 0 0) (Direction 0 0 1) 30 30 (RGB 255 10 10) (0.85,0,0) 0 0) -- Izq
-- rect2 :: Shape
rect2 = Rectangle (Rectangulo (Point3D 0 0 (-15)) (Direction 0 0 (1)) (Direction (-1) 0 0) 30 30 (RGB 100 100 100) (0.85,0,0) 0 0) -- Fondo
-- rect3 :: Shape
rect3 = Rectangle (Rectangulo (Point3D 0 15 (-0)) (Direction 0 (-1) 0) (Direction (-1) 0 0) 40 40  (RGB 100 100 100) (0.85,0,0) 0 0)--Techo
-- Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int
cilindro :: Shape
cilindro = Cylinder (Cilindro centr (Direction 0 1 0) 5 (RGB 200 0 200) (0, 0.0, 0.0) 0 0)
cono = Cone(Cono (Point3D 0 0 (-8)) 1 1 (RGB 200 0 200) (0.85, 0.0, 0.0) 0 0)
tri = Triangle(Triangulo (Point3D (-2) 0 (-8)) (Point3D 0 4 (-8)) (Point3D (2) 0 (-8)) (Point2D 0 1) (Point2D 1 0) (Point2D 0 0) (RGB 200 0 200) (0.85, 0.0, 0.0) 0 0)
camara :: Camara
camara = Camara cam basCam

camara' :: Camara
camara' = Camara (Point3D 0.5 0.5 20) basCam

fmx :: Float
fmx = intMxSim

bola'' = Sphere (Esfera (Point3D (-6) (-10) (-3)) 2 (RGB 200 200 200) (0.85,0,0) 1 0)
bola''' = Sphere (Esfera (Point3D (-2) (-10) (-6)) 2 (RGB 200 200 200) (0.85,0,0) 1 0)
bola'''' = Sphere (Esfera (Point3D (-10) (-10) (0)) 2 (RGB 200 200 200) (0.85,0,0) 1 0)
bolo = Sphere (Esfera (Point3D (0) (0) (0)) 180 (RGB 200 200 200) (0.85,0,0) 1 0)
bolo' = Sphere (Esfera (Point3D (6) (-10) (-14)) 2 (RGB 200 200 200) (0.85,0,0) 1 0)
bolo'' = Sphere (Esfera (Point3D (10) (-10) (-18)) 2 (RGB 200 200 200) (0.85,0,0) 1 0)


figuras :: Set.Set Shape
figuras = Set.fromList $ addFigMult [plano0,plano1,rect2,bola,plano2,bola',plano4,bola] []

-- a :: Shape
-- a =  Sphere (Esfera (Point3D 5 5 (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

-- b :: Shape
-- b =  Sphere (Esfera (Point3D 5 (-5) (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

-- c :: Shape
-- c =  Sphere (Esfera (Point3D (-5) (-5) (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

-- figuras :: Set.Set Shape
-- figuras = Set.fromList $ addFigMult [a, b, c, plano3] []

luces :: [Luz]
luces = [luz]