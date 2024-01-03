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
      rotatePoint,movePoint, divRGB
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
      chunksOf, mulCam )
import PathTracer (pathTracer, luzDirecta, luzArea)
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

{-# INLINE listRayToRGB #-}
listRayToRGB :: [Luz] -> [Shape] -> [Ray] -> StdGen -> Int -> [RGB]
listRayToRGB luz figuras rayos gen nRay = colorDirecto
  --zipWith (+) colorDirecto $ map (`divRGB` fromIntegral ppp) colorIndirecto
  -- map (`divRGB` fromIntegral ppp) colorArea --zipWith (+) colorDirecto $ map (`divRGB` fromIntegral ppp) colorIndirecto
  where
    !antial = map (antialiasing nRay) $ parametricShapeCollision figuras rayos
    rayColisions = listRay antial
    
    (gens, _) = splitAt (length rayColisions * ppp) $ drop 1 $ iterate (snd . split) gen  -- Semillas

    ppp = 10 -- Caminos por pixel
    colorIndirecto = zipWith (pathTracer 1 luz figuras ppp) rayColisions gens
    colorDirecto = map (luzDirecta luz figuras) rayColisions
    colorArea = zipWith (luzArea figuras ppp) rayColisions gens


listRayPhoton :: KdTree Float Foton -> Point3D -> [Shape] -> [Ray] -> Int -> [RGB]
listRayPhoton kdt cam figuras rayos nRay = map (photonMap kdt radio figuras) rayColisions
  where
    !raySMPP = map (antialiasing nRay) $ parametricShapeCollision figuras rayos
    rayColisions = listRay raySMPP
    radio = 10


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
      let vertices1' = map (escalatePointt (1)) vertices1
          customTriangles1 = convertToCustomFormat (vertices1', triangles1)
          boundingVol = buildBVH 4000 customTriangles1
          !figuras' =  addFigMult [(Acelerator boundingVol)]  figuras
          
      -- let !kdt = createKD $ createPhoton potf [] n figuras luces gen'
      -- writeObject "test.bin" kdt
      !notkdt <- readObject "./kd.bin"
      let !kdt = createKD notkdt
      let cams = mulCam camara 0
      let !rayitos = map (\camara -> generateRaysForPixels (maxN*etapasY) etapasX n' etapaX camara (pix*aspectR) pix nRay gen) cams
          -- a = map (\rayos -> listRayPhoton kdt cam figuras' rayos nRay) rayitos
          a = map (\rayos -> listRayToRGB luces figuras rayos gen' nRay) rayitos
          c = mediaLRGB a
          fin = concatMap rgbToString (gammaFunc fmx gamma c)

      writePPM ("a" ++ show n ++ "_" ++ show etapaY ++ "_" ++ show etapaX ++ ".ppm") (round $ pix*aspectR) (round pix) fin


      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Float
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"

    _ -> do
      putStrLn "Please provide an integer as the first argument."
      exitFailure
