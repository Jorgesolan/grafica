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
pix = 512
piCam :: Float
piCam = 25
gamma :: Float
gamma = 2.4
maxN :: Int
maxN = 8
etapasY :: Int
etapasY = 4
etapasX:: Int
etapasX= 2
nRay :: Int
nRay = 1
intMxSim :: Float
intMxSim = 1.0

basCam :: Base
basCam = Base (Direction (piCam*aspectR) 0 0) (Direction 0 piCam 0) (Direction 0 0 (-22))

cam :: Point3D
cam =  Point3D 0 0 30

-- ESCENA
centr :: Point3D
centr = Point3D (-12) (-10) (-14)
centr' :: Point3D
centr' = Point3D 14 (-15) (-10)
centr'' :: Point3D
centr'' = Point3D 0 (-16) (-20)
centr''' :: Point3D
centr''' = Point3D 0 25 (-25)
luz :: Luz
luz = Luz (Point3D 0 17 (-10)) (RGB 255 255 255) intMxKd
luz' :: Luz
luz' = Luz (Point3D 0 0 50) (RGB 255 255 255) 0.70
luz'' :: Point3D
luz'' = Point3D 10 14 (-2)

plano0 :: Shape
plano0 = Plane (Plano (Point3D (-25) 0 0) (Direction 1 0 0) (RGB 250 255 10) (0.8,0,0) 0 0) --Izq
plano1 :: Shape
plano1 =  Plane (Plano (Point3D 25 0 0) (Direction 1 0 0) (RGB 122 10 255) (0.8,0,0) 0 0) -- Der
plano2 :: Shape
plano2 =  Plane (Plano (Point3D 0 25 0) (Direction 0 1 0) (RGB 150 150 150) (0.8,0,0) 0 0) -- Techo
plano3 :: Shape
plano3 =  Plane (Plano (Point3D 0 0 (-25)) (Direction 0 0 1) (RGB 150 150 150) (0.8,0,0) 0 0) -- Fondo
plano4 :: Shape
plano4 =  Plane (Plano (Point3D 0 (-20) 0) (Direction 0 1 0) (RGB 200 200 200) (0.7,0,0.2) 0 0) -- Suelo
plano5 :: Shape
plano5 =  Plane (Plano (Point3D 0 0 50.5) (Direction 0 0 1) (RGB 1 1 1) (0.8,0,0) 0 0) -- Detras Camara
bola :: Shape
bola =  Sphere (Esfera centr 6 (RGB 145 235 249) (0.6, 0, 0.38) 0 0)
bola' :: Shape
bola' =  Sphere (Esfera centr' 5 (RGB 255 255 255) (0, 0.65, 0.2) 1.5 0)
bola'' :: Shape
bola'' = Sphere (Esfera centr'' 4 (RGB 140 140 142) (0.6, 0, 0.3) 0 0)
rect0 :: Shape
rect0 = Rectangle (Rectangulo (Point3D 25 0 0) (Direction 1 0 0) 50 50 (RGB 122 10 255) (0.8,0,0) 0 0)
rect1 :: Shape
rect1 = Rectangle (Rectangulo (Point3D (-25) 0 0) (Direction (-1) 0 0) 50 50 (RGB 250 255 10) (0.8,0,0) 0 0)
rect2 :: Shape
rect2 = Rectangle (Rectangulo (Point3D 0 0 (-25)) (Direction 0 0 (1)) 50 50 (RGB 0 0 0) (0.8,0,0) 0 0)
-- Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int
cilindro :: Shape
cilindro = Cylinder (Cilindro centr (Direction 0 1 0) 5 (RGB 200 0 200) (1.0, 0.0, 0.0) 0 0)
camara :: Camara
camara = Camara cam basCam

fmx :: Float
fmx = 255 * intMxSim
-- Asi pasamos de imagen HDR a HD y tenemos mas margen de maniobra para otras luces

figuras :: [Shape]
figuras = addFigMult [bola,bola',rect0,rect1,rect2,plano2, plano4,plano5] []
-- Poner primero las bolas por la cosa del cristal, modificar el valor de dir Cristal depende del numero de bolas
luces :: [Luz]
luces = [luz{- , luz' -}]