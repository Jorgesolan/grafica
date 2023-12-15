import Elem3D
    ( Luz(Luz), 
    RGB(RGB), 
    Direction(Direction), 
    Point3D(Point3D), 
    escalatePoint, 
    degToRad,
    rotatePoint,movePoint
    )
import Files (writePPM, rgbToString,writeObject)
import Tone_map (gammaFunc,clamp)
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
import Funciones (sumFlLuz)
import PathTracer (pathTracer)
import KdTest ()
import PhotonMap ( createPhoton )
import Data.KdTree.Static ()

import System.Random (StdGen, newStdGen, split)
import Data.List (transpose)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)

-- make clean && make sim && ./sim -N12 && convert a.ppm out.bmp
-- make clean && make sim && ./sim  +RTS -N -l -RTS && convert a.ppm out.bmp
-- make clean && make simulacion && cd ./tmp && ./run.sh && cd .. && convert ./tmp/output.ppm a.bmp


-- Hacer funcion para a un nuevo objeto, meterle el RGB

intMx :: Float
intMx = 50
n :: Float
n=30000
nRebotes :: Int
nRebotes = 8

centr :: Point3D
centr = Point3D (-10) (-10) (-4)
centr' :: Point3D
centr' = Point3D 14 (-15) (-0)
centr'' :: Point3D
centr'' = Point3D 0 (-0) (-0)
centr''' :: Point3D
centr''' = Point3D 0 25 (-25)
luz :: Luz
luz = Luz (Point3D 0 17 0) (RGB 255 255 255) intMx
luz' :: Luz
luz' = Luz (Point3D 10 13 0) (RGB 255 255 255) intMx
luz'' :: Point3D
luz'' = Point3D 10 14 (-2)

plano0 :: Shape
plano0 = Plane (Plano (Point3D (-25) 0 0) (Direction 1 0 0) (RGB 250 255 10) (0.8,0,0) 0 0) --Izq
plano1 :: Shape
plano1 =  Plane (Plano (Point3D 25 0 0) (Direction 1 0 0) (RGB 122 10 255) (0.8,0,0) 0 0) -- Der
plano2 :: Shape
plano2 =  Plane (Plano (Point3D 0 25 0) (Direction 0 1 0) (RGB 150 150 150) (0.8,0,0) 0 0) -- Techo
plano3 :: Shape
plano3 =  Plane (Plano (Point3D 0 0 (-25)) (Direction 0 0 1) (RGB 200 200 200) (0.8,0,0) 0 0) -- Fondo
plano4 :: Shape
plano4 =  Plane (Plano (Point3D 0 (-20) 0) (Direction 0 1 0) (RGB 150 150 150) (0.7,0,0.2) 0 0) -- Suelo
plano5 :: Shape
plano5 =  Plane (Plano (Point3D 0 0 50.5) (Direction 0 0 1) (RGB 0 0 0) (0.8,0,0) 0 0) -- Detras Camara
bola :: Shape
bola =  Sphere (Esfera centr 6 (RGB 145 235 249) (0.6, 0, 0.38) 0 0)
bola' :: Shape
bola' =  Sphere (Esfera centr' 5 (RGB 255 255 255) (0, 0.65, 0.2) 1.5 0)
bola'' :: Shape
bola'' = Sphere (Esfera centr'' 4 (RGB 140 140 142) (0.6, 0, 0.3) 0 0)
rect0 :: Shape
rect0 = Rectangle (Rectangulo (Point3D 25 0 0) (Direction 1 0 0) 50 50 (RGB 250 255 10) (0.8,0,0) 0 0)
rect1 :: Shape
rect1 = Rectangle (Rectangulo (Point3D (-25) 0 0) (Direction 1 0 0) 50 50 (RGB 122 10 255) (0.8,0,0) 0 0)
rect2 :: Shape
rect2 = Rectangle (Rectangulo (Point3D 0 0 (-25)) (Direction 0 0 (1)) 50 50 (RGB 0 0 0) (0.8,0,0) 0 0)
-- bola'' :: Shape
-- bola'' =  Sphere (Esfera centr' 2 (RGB 10 150 240) (0, 0, 0) 0 0)
--bola'' =  Sphere (Esfera centr' 2 (RGB 10 150 240) (0, 1.5, 0) 0)


figuras :: [Shape]
figuras = addFigMult [bola,bola',rect0,rect1,rect2,plano2, plano4,plano5] []
-- Poner primero las bolas por la cosa del cristal, modificar el valor de dir Cristal depende del numero de bolas
luces :: [Luz]
luces = [luz{- , luz' -}]


main :: IO ()
main = do
  start <- getCPUTime
  gen <- newStdGen
  gen' <- newStdGen

  let objFilePath = "../meshes/slab.obj"  
  (vertices, triangles) <- loadObjFile objFilePath
  let vertices' = map (movePoint (Direction 0 24.9 (-20)).escalatePoint 5) vertices
  let customTriangles = convertToCustomFormat (vertices', triangles)
  let triangleLus = encenderShapes customTriangles
  --let figuras' = addFigMult triangleLus figuras

  let kdt =  createPhoton (sumFlLuz luces) [] 0 (round n) figuras luces gen' nRebotes
  print $ length kdt
  writeObject "test.bin" kdt
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) :: Float
  putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"
