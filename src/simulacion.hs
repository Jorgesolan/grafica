{-# LANGUAGE BangPatterns #-}
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
import Files (writePPM, rgbToString, readObject)
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
import Funciones
    ( mediaLRGB,
      generateRaysForPixels,
      obtenerPrimeraColision,
      listRay,
      chunksOf )
import PathTracer (pathTracer)
import KdTest ( createKD )
import PhotonMap ( photonMap )
import Data.KdTree.Static ( KdTree )

import Debug.Trace (trace,traceEventIO)
import System.Random (StdGen, newStdGen, split)
import Data.List (transpose)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

-- make clean && make simulacion && cd ./tmp && ./run.sh && cd .. && convert ./tmp/output.ppm a.bmp


{-# INLINE antialiasing #-}
antialiasing :: Int -> [(Float, Obj)] ->  [(Float, Obj)]
antialiasing n rayos = map obtenerPrimeraColision (chunksOf n rayos) -- Obtiene la colision mas cercana de cada lista de colisiones dependiendo del numero de rayos del antialiasing

listRayToRGB :: [Luz] -> Point3D -> [Shape] -> [Ray] -> StdGen -> StdGen -> Int -> Int -> [[RGB]]
listRayToRGB luz cam figuras rayos gen0 gen1 nRay iter
  | iter == 0 = []
  | otherwise = luzFinal : listRayToRGB luz cam figuras rayos gen1' gen1'' nRay (iter-1)
  where
    (gens, _) = splitAt (length rayos) $ tail $ iterate (snd . split) gen0
    rayColisions = listRay $ map (antialiasing nRay) $ parametricShapeCollision figuras rayos
    nRebotes = 0
    luzFinal = zipWith (pathTracer 1 luz figuras 0 nRebotes) rayColisions gens
    gen1' = fst $ split gen1
    gen1'' = snd $ split gen1

listRayPhoton :: KdTree Float Foton -> StdGen -> Point3D -> [Shape] -> [Ray] -> Int -> [RGB]
listRayPhoton kdt gen cam figuras rayos nRay = map (photonMap kdt radio figuras gen) rayColisions
  where
    !raySMPP = map (antialiasing nRay) $ parametricShapeCollision figuras rayos
    rayColisions = listRay raySMPP
    radio = 10

listaRaySupreme :: [Luz] -> Point3D -> [Shape] -> [Ray] -> StdGen -> StdGen -> Int -> [RGB]
listaRaySupreme luz cam figuras rayos gen gen' nRay = luzFinal
  where
    nIter = 1
    luzFinal = mediaLRGB $ listRayToRGB luz cam figuras rayos gen gen' nRay nIter

aspectR :: Float
aspectR = 16/9
pix :: Float
pix = 2160
piCam :: Float
piCam = 25
gamma :: Float
gamma = 2.4
maxN :: Int
maxN = 72
etapas :: Int
etapas = 30
nRay :: Int
nRay = 2000
intMx :: Float
intMx = 1.0

basCam :: Base
basCam = Base (Direction (piCam*aspectR) 0 0) (Direction 0 piCam 0) (Direction 0 0 (-22))

cam :: Point3D
cam =  Point3D 0 0 30

centr :: Point3D
centr = Point3D (-12) (-10) (-14)
centr' :: Point3D
centr' = Point3D 14 (-15) (-10)
centr'' :: Point3D
centr'' = Point3D 0 (-16) (-20)
centr''' :: Point3D
centr''' = Point3D 0 25 (-25)
luz :: Luz
luz = Luz (Point3D 0 17 0) (RGB 255 255 255) intMx
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
rect0 = Rectangle (Rectangulo (Point3D 25 0 0) (Direction 1 0 0) 50 50 (RGB 250 255 10) (0.8,0,0) 0 0)
rect1 :: Shape
rect1 = Rectangle (Rectangulo (Point3D (-25) 0 0) (Direction 1 0 0) 50 50 (RGB 122 10 255) (0.8,0,0) 0 0)
rect2 :: Shape
rect2 = Rectangle (Rectangulo (Point3D 0 0 (-25)) (Direction 0 0 (1)) 50 50 (RGB 0 0 0) (0.8,0,0) 0 0)
-- Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int
cilindro :: Shape
cilindro = Cylinder (Cilindro centr (Direction 0 1 0) 5 (RGB 200 0 200) (1.0, 0.0, 0.0) 0 0)
camara :: Camara
camara = Camara cam basCam

fmx :: Float
fmx = 255 * intMx
-- Asi pasamos de imagen HDR a HD y tenemos mas margen de maniobra para otras luces

figuras :: [Shape]
figuras = addFigMult [bola,bola',rect0,rect1,rect2,plano2, plano4,plano5] []
-- Poner primero las bolas por la cosa del cristal, modificar el valor de dir Cristal depende del numero de bolas
luces :: [Luz]
luces = [luz{- , luz' -}]

main :: IO ()
main = do
  args <- getArgs
  case map readMaybe args of
    [Just nStr, Just mStr] -> do
      gen <- newStdGen
      gen' <- newStdGen
      let n = nStr :: Int
      let etapa = mStr :: Int
      let n' = n + (etapa * maxN)
      putStrLn $ "The value of 'n' is: " ++ show n'
      start <- getCPUTime
      -- let objFilePath = "../meshes/slab.obj"  
      -- (vertices, triangles) <- loadObjFile objFilePath
      -- let vertices' = map (movePoint (Direction 0 24.9 (-20)).escalatePoint 5) vertices
      --     customTriangles = convertToCustomFormat (vertices', triangles)
      --     trianglesLus = encenderShapes customTriangles
          --figuras' = addFigMult trianglesLus figuras

      -- let objFilePath1 = "cubo.obj"  
      -- (vertices1, triangles1) <- loadObjFile objFilePath1
      -- let vertices1' = map (rotatePoint 'X' 0.movePoint (Direction (-5) 0 0).escalatePoint (2.5)) vertices1
      --     customTriangles1 = convertToCustomFormat (vertices1', triangles1)
      --     !figuras' = {- addFigMult (customTriangles ++ customTriangles1) -} figuras
      -- let !kdt = createKD $ createPhoton potf [] n figuras luces gen'
      -- writeObject "test.bin" kdt
      !notkdt <- readObject "../src/test.bin"
      let !kdt = createKD notkdt
      let !rayitos = generateRaysForPixels (maxN*etapas) n' camara (pix*aspectR) pix nRay gen
          a = listRayPhoton kdt gen' cam figuras rayitos nRay
          fin = concatMap rgbToString (gammaFunc fmx gamma a)

      writePPM ("a" ++ show n ++ "_" ++ show etapa ++ ".ppm") (round $ pix*aspectR) (round pix) fin


      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Float
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"

    _ -> do
      putStrLn "Please provide an integer as the first argument."
      exitFailure
