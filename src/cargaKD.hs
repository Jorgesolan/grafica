{-# LANGUAGE BangPatterns #-}
import Elem3D 
import Files (writePPM, rgbToString,writeObject)
import Tone_map (gammaFunc,clamp)
import Figuras
import Funciones
import PathTracer (pathTracer)
import KdTest
import PhotonMap
import Data.KdTree.Static 

import Debug.Trace (trace,traceEventIO)
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

intMx = 1000.0
n=20000

centr = Point3D (-10) (-10) (-4)
centr' = Point3D 13 (-15) (-2)
luz = Luz (Point3D (0) 15 (0)) (RGB 255 255 255) intMx
luz' = Luz (Point3D (0) 0 (50)) (RGB 255 255 255) 0.70
luz'' = Point3D 10 14 (-2)

plano0 = Plane (Plano (Point3D (-20) 0 0) (Direction 1 0 0) (RGB 250 255 10) (1, 0, 0) 0) --Izq
plano1 =  Plane (Plano (Point3D 20 0 0) (Direction 1 0 0) (RGB 122 10 255) (1, 0, 0) 0) -- Der
plano2 =  Plane (Plano (Point3D 0 25 0) (Direction 0 1 0) (RGB 150 150 150) (1, 0, 0) 0) -- Techo
plano3 =  Plane (Plano (Point3D 0 0 (-25)) (Direction 0 0 1) (RGB 150 150 150) (1, 0, 0) 0) -- Fondo
plano4 =  Plane (Plano (Point3D 0 (-20) 0) (Direction 0 1 0) (RGB 150 150 150) (1, 0, 0) 0) -- Suelo
plano5 =  Plane (Plano (Point3D 0 0 (50.5)) (Direction 0 0 1) (RGB 0 0 0) (1, 0, 0) 0) -- Detras Camara
bola =  Sphere (Esfera centr 6 (RGB 255 10 10) (1, 0, 0) 0)
bola' =  Sphere (Esfera centr' 5 (RGB 10 150 240) (1, 0, 0) 0)
bola'' =  Sphere (Esfera centr' 2 (RGB 10 150 240) (0, 0, 0) 0)
--bola'' =  Sphere (Esfera centr' 2 (RGB 10 150 240) (0, 1.5, 0) 0)


figuras = addFigMult [bola,bola',plano0,plano1,plano2,plano3, plano4,plano5] [] 
-- Poner primero las bolas por la cosa del cristal, modificar el valor de dir Cristal depende del numero de bolas
luces = [luz{- , luz' -}]
potf = 4.0*pi*intMx/n
      
main :: IO ()
main = do
  start <- getCPUTime 
  gen <- newStdGen
  gen' <- newStdGen
  
  let kdt =  createPhoton potf [] n figuras luces gen'
  print $ length kdt
  writeObject "test.bin" kdt
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) :: Float
  putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"
