{-# LANGUAGE BangPatterns #-}
import Escena
import Elem3D
    ( Foton,
      Luz(..),
      RGB(..),
      Base(Base),
      Ray,
      Direction(Direction),
      Point3D(..),
      escalatePoint,escalatePointt,
      degToRad,
      rotatePoint,movePoint
    )

import Figuras
    ( Obj,
      Shape(Sphere, Plane, Cylinder,Rectangle,Acelerator),
      Plano(Plano),
      Esfera(Esfera),
      Cilindro(Cilindro),
      Rectangulo(Rectangulo),
      Camara(Camara),
      BVH(BVH),
      addFigMult,
      parametricShapeCollision,
      loadObjFile,
      convertToCustomFormat, encenderShape ,encenderShapes, buildBVH
      )
import Files (writePPM, rgbToString, readObject)
import Tone_map (gammaFunc,clamp)
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

-- make clean && make cargaKD && cd ./tmp && ./cargaKD && cd ..
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
    nRebotes = 1
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
    nIter = 3
    luzFinal = mediaLRGB $ listRayToRGB luz cam figuras rayos gen gen' nRay nIter



main :: IO ()
main = do
  args <- getArgs
  case map readMaybe args of
    [Just nStr, Just mStr, Just oStr] -> do
      gen <- newStdGen
      gen' <- newStdGen
      let n = nStr :: Int
      let etapaY = mStr :: Int
      let etapaX = oStr :: Int
      let n' = n + (etapaY * maxN)
      putStrLn $ "The value of 'n' is: " ++ show n'
      start <- getCPUTime
      -- let objFilePath = "../meshes/slab.obj"  
      -- (vertices, triangles) <- loadObjFile objFilePath
      -- let vertices' = map (movePoint (Direction 0 24.9 (-20)).escalatePoint 5) vertices
      --     customTriangles = convertToCustomFormat (vertices', triangles)
      --     trianglesLus = encenderShapes customTriangles
          --figuras' = addFigMult trianglesLus figuras

      let objFilePath1 = "../meshes/simplef15.obj"  
      (vertices1, triangles1) <- loadObjFile objFilePath1
      let vertices1' = map (movePoint (Direction (0) (-5) (-25)).rotatePoint 'X' (370).rotatePoint 'Y' 90.rotatePoint 'X' 90.movePoint (Direction (0) 0 0).escalatePointt (1)) vertices1
          customTriangles1 = convertToCustomFormat (vertices1', triangles1)
          boundingVol = buildBVH customTriangles1
          !figuras' =  addFigMult [(Acelerator boundingVol)]  figuras
          
      -- let !kdt = createKD $ createPhoton potf [] n figuras luces gen'
      -- writeObject "test.bin" kdt
      !notkdt <- readObject "./kd.bin"
      let !kdt = createKD notkdt
      let !rayitos = generateRaysForPixels (maxN*etapasY) etapasX n' etapaX camara (pix*aspectR) pix nRay gen
          a = listRayPhoton kdt gen' cam figuras' rayitos nRay
          -- a = listaRaySupreme luces cam figuras rayitos gen gen' nRay
          fin = concatMap rgbToString (gammaFunc fmx gamma a)

      writePPM ("a" ++ show n ++ "_" ++ show etapaY ++ "_" ++ show etapaX ++ ".ppm") (round $ pix*aspectR) (round pix) fin


      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Float
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"

    _ -> do
      putStrLn "Please provide an integer as the first argument."
      exitFailure
