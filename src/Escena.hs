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
      rotatePoint,movePoint,rotatePointt,pointDir
    )

import Figuras
    ( Obj, Point2D(..), Img(..),
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
n=75000
nRebotes :: Int
nRebotes = 6
-- SIMULACION
aspectR :: Float
aspectR = 1
pix :: Float
pix = 300
piCam :: Float
piCam = 12.5
gamma :: Float
gamma = 2.4
maxN :: Int
maxN = 12
etapasY :: Int
etapasY = 1
etapasX:: Int
etapasX = 4
nRay :: Int
nRay = 1
intMxSim :: Float
intMxSim = 1.0

basCam :: Base
basCam = Base (Direction (piCam*aspectR) 0 0) (Direction 0 piCam 0) (Direction 0 (0) (-15))

cam :: Point3D
cam =  Point3D 0 0 (-20)

-- ESCENA
centr :: Point3D
centr = Point3D (-9) (-8) (1)
-- centr' :: Point3D
centr' = Point3D (10) (-8) (2)
-- centr'' :: Point3D
-- centr'' = Point3D 0 (-15) (-44)
-- centr''' :: Point3D
-- centr''' = Point3D 15 (-10) (-22)


luzI = Luz (extraR $ Point3D (4.7) 2.5 (0)) (RGB 255 220 175) (intMxKd*0.9) -- Dentro
luzI' = Luz (extraR $ Point3D (5) 2.5 (9)) (RGB 255 220 175) intMxKd -- Dentro
luzI'' = Luz (extraR $ Point3D (5) 2.5 (18)) (RGB 255 220 175) intMxKd -- Dentro
luzI''' = Luz (extraR $ Point3D (-7) 2.8 (-20)) (RGB 255 220 175) intMxKd -- Dentro
luzD = Luz (extraR $ Point3D (-7) 2.5 (0)) (RGB 255 220 175) intMxKd -- Dentro
luzD' = Luz (extraR $ Point3D (-7) 2.5 (9)) (RGB 255 220 175) intMxKd -- Dentro
luzD'' = Luz (extraR $ Point3D (-7) 2.5 (18)) (RGB 255 220 175) intMxKd -- Dentro
-- luz = Luz (Point3D (0) 0 (10)) (RGB 255 255 255) intMxKd -- Dentro

luzE = Luz (extraR $ Point3D (17) 3 (7)) (RGB 125 125 255) (intMxKd) -- LUZ 1º VENTANA
luzE' = Luz (extraR $ Point3D (17) 3 (-8.5)) (RGB 125 125 255) (intMxKd) -- LUZ 1º VENTANA


luz' :: Luz
luz' = Luz (Point3D (30) (10) (-9)) (RGB 150 150 255) (intMxKd) -- LUZ 1º VENTANA
-- luz'' :: Luz
-- luz'' = Luz (Point3D (25) (5) (11)) (RGB 150 150 255) (intMxKd**1.5) -- LUZ 2º VENTANA
-- luz''' :: Luz
-- luz''' = Luz (Point3D 0 0 (30))  (RGB 255 255 255) intMxKd -- Al lado de columna
-- luz'''' = Luz (Point3D (0) 2.5 (-15)) (RGB 255 255 255) intMxKd -- Dentro
luz''''' = Luz (Point3D (0) 2 (10)) (RGB 255 220 175) intMxKd 
luz'''''' = Luz (Point3D (6) 2.5 (-19)) (RGB 255 255 255) intMxKd -- Dentro
extraR=rotatePointt 'Y' (15)
bola :: Shape
bola =  Sphere (Esfera (extraR $ Point3D (0) 2.5 (0)) 1 (RGB 255 255 255) (0.9, 0,0) 1 0 Nada ) -- centro
bola' =  Sphere (Esfera (extraR $ Point3D (0) 2.5 (-18)) 1 (RGB 255 0 0) (0.9, 0,0) 1 0 Nada) -- principio 
bola'' =  Sphere (Esfera (extraR $ Point3D (0) 2.5 (22)) 1 (RGB 0 0 255) (0.9, 0,0) 1 0 Nada) -- fondo
bola''' =  Sphere (Esfera (extraR $ Point3D (-7.5) 0 (0)) 1 (RGB 255 0 0) (0.9, 0,0) 1 0 Nada) -- derecha
bola'''' =  Sphere (Esfera (extraR $ Point3D (15) 2.5 (0)) 1 (RGB 0 0 255) (0.9, 0,0) 1 0 Nada) -- izqd

bolav =  Sphere (Esfera (extraR $ Point3D (18) 3 (7)) 1 (RGB 0 255 0) (0.9, 0,0) 1 0 Nada) -- izqd
bolav' =  Sphere (Esfera (extraR $ Point3D (18) 3 (-8.5)) 1 (RGB 255 0 0) (0.9, 0,0) 1 0 Nada) -- izqd
bolaO =  Sphere (Esfera (extraR $ Point3D (0) 0 (0)) 30 (RGB 0 0 0) (0.9, 0,0) 1 0 Nada ) -- centro

-- rectcow = Rectangle (Rectangulo (extraR $ Point3D 0 5 (-2)) (pointDir.extraR $ Point3D (0) 0 (1)) (pointDir.extraR $ Point3D (-1) 0 (0)) 3.74 6.26 (RGB 100 100 100) (0.85,0,0) 0 0 Nada) -- Der
-- rectrom = Rectangle (Rectangulo (extraR $ Point3D 0 1 20) (pointDir.extraR $ Point3D (0) 0 (-1)) (pointDir.extraR $ Point3D (-1) 0 (0)) 5.05 8.95 (RGB 100 100 100) (0.85,0,0) 0 0 Nada) 

rectrom = Rectangle (Rectangulo (extraR $ Point3D 0 1 (5)) (pointDir.extraR $ Point3D (0) 0 (1)) (pointDir.extraR $ Point3D (-1) 0 (0)) 5.05 8.95 (RGB 100 100 100) (0.85,0,0) 0 0 Nada) 
-- bolal =  Sphere (Esfera (extraR $ Point3D (5.75) 2.5 (0)) 1 (RGB 255 0 0) (0.9, 0,0) 1 0) -- centro
-- bolal' =  Sphere (Esfera (extraR $ Point3D (5.75) 2.5 (9)) 1 (RGB 0 255 0) (0.9, 0,0) 1 0) -- centro
-- bolal'' =  Sphere (Esfera (extraR $ Point3D (5.75) 2.5 (18)) 1 (RGB 0 0 255) (0.9, 0,0) 1 0) -- centro
plano0 :: Shape
plano0 = Plane (Plano (Point3D (-29) 0 0) (Direction 1 0 0) (RGB 100 100 100) (0.85,0,0) 0 0 Nada) --Izq
plano1 :: Shape
plano1 =  Plane (Plano (extraR $ Point3D 24 0 0) (pointDir.extraR $ Point3D 1 0 0) (RGB 100 100 100) (0.85,0,0) 0 0 Nada) -- Der
plano2 :: Shape
plano2 =  Plane (Plano (Point3D 0 15 0) (Direction 0 1 0) (RGB 100 100 100) (0.85,0,0) 0 0 Nada) -- Techo
plano3 :: Shape
plano3 =  Plane (Plano (Point3D 0 0 (-15)) (Direction 0 0 (-1)) (RGB 1 1 1) (0.85,0,0) 0 0 Nada) -- Fondo
-- plano4 :: Shape
plano4 =  Plane (Plano (Point3D 0 (-15) 0) (Direction 0 1 0) (RGB 100 100 100) (0.85,0,0) 1.6 0 Nada) -- Suelo
plano5 :: Shape
plano5 =  Plane (Plano (Point3D 0 0 50.5) (Direction 0 0 1) (RGB 1 1 1) (0.85,0,0) 0 0 Nada) -- Detras Camara


-- rect0 :: Shape
--rect0 = Rectangle (Rectangulo (Point3D 15 0 (-10)) (Direction (1) 0 0) (Direction 0 0 (-1)) 30 30 (RGB 10 255 10) (0.85,0,0) 0 0 Nada) -- Der
-- rect1 :: Shape
rect1 = Rectangle (Rectangulo (Point3D (-15) 0 (-10)) (Direction 1 0 0) (Direction 0 0 1) 30 30 (RGB 255 10 10) (0.85,0,0) 0 0 Nada) -- Izq
-- rect2 :: Shape
rect2 = Rectangle (Rectangulo (Point3D 0 0 (-15)) (Direction 0 0 (1)) (Direction (-1) 0 0) 30 30 (RGB 100 100 100) (0.85,0,0) 0 0 Nada) -- Fondo
-- rect3 :: Shape
rect3 = Rectangle (Rectangulo (Point3D 0 15 (-0)) (Direction 0 (-1) 0) (Direction (-1) 0 0) 40 40  (RGB 100 100 100) (0.85,0,0) 0 0 Nada)--Techo
-- Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int
-- cilindro :: Shape
-- cilindro = Cylinder (Cilindro centr (Direction 0 1 0) 5 (RGB 200 0 200) (0, 0.0, 0.0) 0 0 Nada)
-- cono = Cone(Cono (Point3D 0 0 (-8)) 1 1 (RGB 200 0 200) (0.85, 0.0, 0.0) 0 0 Nada)
-- tri = Triangle(Triangulo (Point3D (-2) 0 (-8)) (Point3D 0 4 (-8)) (Point3D (2) 0 (-8)) (Point2D 0 1) (Point2D 1 0) (Point2D 0 0) (RGB 200 0 200) (0.85, 0.0, 0.0) 0 0 Nada)
camara :: Camara
camara = Camara cam basCam

camara' :: Camara
camara' = Camara (Point3D 0.5 0.5 20) basCam

fmx :: Float
fmx = intMxSim




figuras :: Set.Set Shape
figuras = Set.fromList $ addFigMult [plano1, bola, {- rectcow, -} rectrom,bolaO {- ,bola',bola'', bola''',bola'''' -}{- ,bolav,bolav' -}{- plano0,plano1,rect2,bola,plano2,bola',plano4,bola -}{- ,bolal,bolal',bolal'' -}] []

-- a :: Shape
-- a =  Sphere (Esfera (Point3D 5 5 (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

-- b :: Shape
-- b =  Sphere (Esfera (Point3D 5 (-5) (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

-- c :: Shape
-- c =  Sphere (Esfera (Point3D (-5) (-5) (-10)) 2 (RGB 200 200 200) (1, 0, 0) 1.7 0)

-- figuras :: Set.Set Shape
-- figuras = Set.fromList $ addFigMult [a, b, c, plano3] []

luces :: [Luz]
-- luces = [luz,luz'''',luz''''',luz'''''']
luces = [luzI,luzI',luzI'',luzD,luzD',luzD'',luzE,luzE',luzI''']