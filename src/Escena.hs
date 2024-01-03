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
    ( Obj,
      Shape(Sphere, Plane, Cylinder,Rectangle),
      Plano(Plano),
      Esfera(Esfera),
      Cilindro(Cilindro),
      Rectangulo(Rectangulo),
      Camara(Camara),
      addFigMult,
      parametricShapeCollision,
      loadObjFile,
      convertToCustomFormat, encenderShape ,encenderShapes
      )
-- CARGAKD
intMxKd :: Float
intMxKd = 50
n :: Float
n=10000
nRebotes :: Int
nRebotes = 8
-- SIMULACION
aspectR :: Float
aspectR = 1
pix :: Float
pix = 256
piCam :: Float
piCam = 12.5
gamma :: Float
gamma = 2.4
maxN :: Int
maxN = 8
etapasY :: Int
etapasY = 1
etapasX:: Int
etapasX= 1
nRay :: Int
nRay = 1
intMxSim :: Float
intMxSim = 1.0

basCam :: Base
basCam = Base (Direction (piCam*aspectR) 0 0) (Direction 0 piCam 0) (Direction 0 0 (12.5))

cam :: Point3D
cam =  Point3D 0 (0) 30

-- ESCENA
centr :: Point3D
centr = Point3D (-2) (-10) (-15)
centr' :: Point3D
centr' = Point3D (9.25) (-9) (-10)
centr'' :: Point3D
centr'' = Point3D 0 (-15) (-44)
centr''' :: Point3D
centr''' = Point3D 15 (-10) (-22)
luz :: Luz
luz = Luz (Point3D 0 8 (-7)) (RGB 255 255 255) 1
luz' :: Luz
luz' = Luz (Point3D 0 0 50) (RGB 255 255 255) 0.70
luz'' :: Point3D
luz'' = Point3D 10 14 (-2)

plano0 :: Shape
plano0 = Plane (Plano (Point3D (-20) 0 0) (Direction 1 0 0) (RGB 250 255 10) (0.8,0,0) 0 0) --Izq
plano1 :: Shape
plano1 =  Plane (Plano (Point3D 20 0 0) (Direction 1 0 0) (RGB 122 10 255) (0.8,0,0) 0 0) -- Der
plano2 :: Shape
plano2 =  Plane (Plano (Point3D 0 10 0) (Direction 0 1 0) (RGB 150 150 150) (0.8,0,0) 0 0) -- Techo
plano3 :: Shape
plano3 =  Plane (Plano (Point3D 0 0 (-20)) (Direction 0 0 (-1)) (RGB 1 1 1) (0.8,0,0) 0 0) -- Fondo
plano4 :: Shape
plano4 =  Plane (Plano (Point3D 0 (-12.5) 0) (Direction 0 1 0) (RGB 200 200 200) (0.7,0,0) 1.6 0) -- Suelo
plano5 :: Shape
plano5 =  Plane (Plano (Point3D 0 0 50.5) (Direction 0 0 1) (RGB 1 1 1) (0.8,0,0) 0 0) -- Detras Camara
bola :: Shape
bola =  Sphere (Esfera centr 2 (RGB 0 200 200) (0.7, 0, 0) 1.5 0)
bola' :: Shape
bola' =  Sphere (Esfera centr' 2.5 (RGB 255 255 255) (0,0.6,0) 1.5 0)
bola'' :: Shape
bola'' =  Sphere (Esfera centr'' 4 (RGB 150 0 0 ) (0.8, 0, 0) 0 0)
bola''' :: Shape
bola''' =  Sphere (Esfera centr''' 5 (RGB 122 210 155) (0.8,0,0) 1.5 0)
rect0 :: Shape
rect0 = Rectangle (Rectangulo (Point3D 12.5 0 (-10)) (Direction (1) 0 0) (Direction 0 0 (-1)) 25 25 (RGB 122 10 255) (0.8,0,0) 0 0)
rect1 :: Shape
rect1 = Rectangle (Rectangulo (Point3D (-12.5) 0 (-10)) (Direction 1 0 0) (Direction 0 0 1) 25 25 (RGB 250 255 10) (0.8,0,0) 0 0)
rect2 :: Shape
rect2 = Rectangle (Rectangulo (Point3D 0 0 (-20)) (Direction 0 0 (1)) (Direction (-1) 0 0) 30 30 (RGB 150 150 150) (0.8,0,0) 0 0)
rect3 :: Shape
rect3 = Rectangle (Rectangulo (Point3D 0 12.5 (-15)) (Direction 0 (-1) 0) (Direction (-1) 0 0) 25 25  (RGB 150 150 150) (0.8,0,0) 0 0)
-- Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int
cilindro :: Shape
cilindro = Cylinder (Cilindro centr (Direction 0 1 0) 5 (RGB 200 0 200) (0, 0.0, 0.0) 0 0)
camara :: Camara
camara = Camara cam basCam

camara' :: Camara
camara' = Camara (Point3D 0.5 0.5 20) basCam

fmx :: Float
fmx = intMxSim

bolo = Sphere (Esfera (Point3D 0 0 10) 15 (RGB 200 200 200) (0,1,0) 1 0)
figuras :: [Shape]
figuras = addFigMult [{- bolo, -} bola', rect0,rect1,rect2,rect3, plano4] []


a :: Shape
a =  Sphere (Esfera (Point3D 5 5 (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

b :: Shape
b =  Sphere (Esfera (Point3D 5 (-5) (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

c :: Shape
c =  Sphere (Esfera (Point3D (-5) (-5) (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

-- figuras :: [Shape] -- Pruebas de niebla
-- figuras = addFigMult [a, b, c, plano3] []

luces :: [Luz]
luces = [luz{- , luz' -}]