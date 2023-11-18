{-# LANGUAGE BangPatterns #-}
import Elem3D 
import Files (writePPM, rgbToString)
import Tone_map (gammaFunc,clamp)
import Figuras
import Funciones
import PathTracer (pathTracer)

import Debug.Trace (trace,traceEventIO)
import System.Random (StdGen, newStdGen, split)
import Data.List (transpose)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)

-- make clean && make sim && ./sim -N12 && convert a.ppm out.bmp
-- make clean && make sim && ./sim  +RTS -N -l -RTS && convert a.ppm out.bmp
-- make clean && make simulacion && cd ./tmp && ./run.sh && cd ..

--en vez de sumRBG es meterle lo de intLuz / ((modd (p #< pointLuz)+1.0)**2) * 4 / pi * abs (norm .* (normal (p #< pointLuz)))
-- con su nuevo rgb y patatas....

-- Hacer funcion para a un nuevo objeto, meterle el RGB

{-# INLINE antialiasing #-}
antialiasing :: Int -> [(Float, Obj)] ->  [(Float, Obj)]
antialiasing n rayos = map (obtenerPrimeraColision) (chunksOf n rayos) -- Obtiene la colision mas cercana de cada lista de colisiones dependiendo del numero de rayos del antialiasing

listRayToRGB :: [Luz] -> Point3D -> [Shape] -> [Ray] -> StdGen -> StdGen -> Int -> Int -> [[RGB]]
listRayToRGB luz cam figuras rayos gen0 gen1 nRay iter 
  | iter == 0 = []
  | otherwise = [luzFinal] ++ listRayToRGB luz cam figuras rayos gen1' gen1'' nRay (iter-1)
  where
    (gens, _) = splitAt (length rayos) $ tail $ iterate (snd . split) gen0
    rayColisions = listRay $ map (antialiasing nRay) $ parametricShapeCollision figuras rayos
    nRebotes = 10
    luzFinal = zipWith (pathTracer luz figuras 0 nRebotes) rayColisions gens
    gen1' = fst $ split gen1
    gen1'' = snd $ split gen1

listaRaySupreme :: [Luz] -> Point3D -> [Shape] -> [Ray] -> StdGen -> StdGen -> Int -> [RGB]
listaRaySupreme luz cam figuras rayos gen gen' nRay = luzFinal
  where
    nIter = 2
    luzFinal = mediaLRGB $ listRayToRGB luz cam figuras rayos gen gen' nRay nIter


pix :: Float
pix = 600
piCam :: Float
piCam = 25
gamma :: Float
gamma = 2.8
maxN = 1
etapas = 1
nRay = 1


basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 25)
centr = Point3D (-5) (-10) 0
centr' = Point3D 13 (-20) (-2)
luz = Luz (Point3D (0) 15 (-4)) (RGB 255 255 255) 1
luz' = Luz (Point3D (0) 0 (50)) (RGB 255 255 255) 0.75
luz'' = Point3D 10 14 (-2)
cam =  Point3D 0 0 50
plano0 = Plane (Plano (Point3D (-20) 0 0) (Direction 1 0 0) (RGB 250 255 10) (1, 0, 0) 0) --Izq
plano1 =  Plane (Plano (Point3D 20 0 0) (Direction (-1) 0 0) (RGB 122 10 255) (1, 0, 0) 1) -- Der
plano2 =  Plane (Plano (Point3D 0 25 0) (Direction 0 (-1) 0) (RGB 200 200 200) (1, 0, 0) 2) -- Techo
plano3 =  Plane (Plano (Point3D 0 0 (-25)) (Direction 0 0 1) (RGB 200 200 200) (1, 0, 0) 3) -- Fondo
plano4 =  Plane (Plano (Point3D 0 (-20) 0) (Direction 0 1 0) (RGB 200 200 200) (1, 0, 0) 4) -- Suelo
plano5 =  Plane (Plano (Point3D 0 0 (51)) (Direction 0 0 1) (RGB 0 0 0) (1, 0, 0) 5) -- Detras Camara
bola =  Sphere (Esfera centr 6 (RGB 255 10 10) (0, 0, 1) 6)
bola' =  Sphere (Esfera centr' 5 (RGB 10 150 240) (0, 0, 1) 7)
-- donut = Donut (Rosquilla centr' (Direction 0 0 (-1)) 20 3 (RGB 10 255 255) 0 0 8)
-- tri1 = Triangle (Triangulo (Point3D 0 100 50) (Point3D 100 100 50) (Point3D 100 0 100) (RGB 60 80 100) 0 8)
camara = Camara cam basCam

figuras = [bola,bola',plano0,plano1,plano2,plano3, plano4,plano5]
luces = [luz, luz']
-- figurasSinPlanos = (parametricShapeCollision [bola,bola',bola''])
main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Just nStr -> do
      gen <- newStdGen
      gen' <- newStdGen
      let !n = read nStr :: Int
      
      putStrLn $ "The value of 'n' is: " ++ show n
      start <- getCPUTime 
      let objFilePath = "diamante.obj"  
      (vertices, triangles) <- loadObjFile objFilePath
      let vertices' = map (rotatePoint 'X' (degToRad 0).movePoint (Direction (-5) 2.5 0).escalatePoint (2.5)) vertices
          customTriangles = convertToCustomFormat (vertices', triangles)

      let objFilePath1 = "cubo.obj"  
      (vertices1, triangles1) <- loadObjFile objFilePath1
      let vertices1' = map (rotatePoint 'X' 0.movePoint (Direction (-5) 0 0).escalatePoint (2.5)) vertices1
          customTriangles1 = convertToCustomFormat (vertices1', triangles1)
          !figuras' = figuras -- ++ customTriangles ++ customTriangles1 -- Al añadir triangulos, jugar con sus ids, son muy importantes

      let rayitos = generateRaysForPixels (maxN*etapas) n camara pix pix nRay gen 
          !a = listaRaySupreme luces cam figuras' rayitos gen gen' nRay
          !fin = concat $ map rgbToString $ gammaFunc 255 gamma a

      writePPM ("a" ++ (show n) ++ ".ppm") (round pix) (round pix) fin
      
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Float
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"

    Nothing -> do
      putStrLn "Please provide an integer as the first argument."
      exitFailure