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
      rotatePoint,movePoint, divRGB,rotatePointt
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
import qualified Data.DList as DL
import qualified Data.Set as Set
-- make clean && make cargaKD && cd ./tmp && ./cargaKD && cd ..
-- make clean && make simulacion && cd ./tmp && ./run.sh && cd .. && convert ./tmp/output.ppm a.bmp

{-# INLINE antialiasing #-}
antialiasing :: Int -> [Obj] -> [Obj]
antialiasing n rayos = map obtenerPrimeraColision $ map Set.fromList (chunksOf n rayos) -- Obtiene la colision mas cercana de cada lista de colisiones dependiendo del numero de rayos del antialiasing

{-# INLINE listRayToRGB #-}
listRayToRGB :: [Luz] -> Set.Set Shape -> [Ray] -> StdGen -> Int -> [RGB]
listRayToRGB luz figuras rayos gen nRay =  colorDirecto --zipWith (+) colorDirecto $ map (`divRGB` fromIntegral ppp) colorIndirecto
  -- map (`divRGB` fromIntegral ppp) colorArea --zipWith (+) colorDirecto $ map (`divRGB` fromIntegral ppp) colorIndirecto
  where
    !antial = map listRay $ parametricShapeCollision figuras rayos
    rayColisions = antialiasing nRay antial
    
    (gens, _) = splitAt (length rayColisions * ppp) $ drop 1 $ iterate (snd . split) gen  -- Semillas

    ppp = 10 -- Caminos por pixel
    colorIndirecto = zipWith (pathTracer 1 luz figuras ppp) rayColisions gens
    colorDirecto = map (luzDirecta luz figuras) rayColisions
    colorArea = zipWith (luzArea figuras ppp) rayColisions gens


listRayPhoton :: KdTree Float Foton -> Point3D -> Set.Set Shape -> [Ray] -> Int -> [RGB]
listRayPhoton kdt cam figuras rayos nRay = map (photonMap kdt radio figuras) rayColisions
  where
    !raySMPP = map listRay $ parametricShapeCollision figuras rayos
    rayColisions = antialiasing nRay raySMPP
    radio = 8


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

      let objFilePath1 = "../meshes/simplepalace.obj"  
      (vertices1, triangles1) <- loadObjFile objFilePath1
      let vertices1' = map (escalatePointt (4).movePoint (Direction 7.5 (-2.5) (-9.75)). rotatePointt 'Y' (282.5) ) vertices1
          customTriangles1 = convertToCustomFormat (vertices1', triangles1)
          boundingVol = buildBVH 4000 customTriangles1
          !figuras' =  Set.fromList $ addFigMult [(Acelerator boundingVol)] $ Set.toList figuras
      let objFilePath2 = "../meshes/simplehaskell.obj"  
      (vertices2, triangles2) <- loadObjFile objFilePath2
      let vertices2' = map (escalatePointt (1).movePoint (Direction (-5) (-5) (-28)). rotatePointt 'Y' (90)) vertices2
          customTriangles2 = convertToCustomFormat (vertices2', triangles2)
          boundingVol' = buildBVH 4000 customTriangles2
          figuras'' =  Set.fromList $ addFigMult [(Acelerator boundingVol')] (Set.toList figuras')    
      
      
      !notkdt <- readObject "./kd.bin" -- Carga la lista de fotones del binario
      let kdt = createKD notkdt -- Crea el kdtree

      let cams = mulCam camara 0 0.75-- El primer número indica el número de muestras que se toman desde la cámara, el segundo el radio de apertura
          rayitos = map (\camara -> generateRaysForPixels (maxN*etapasY) etapasX n' etapaX camara (pix*aspectR) pix nRay gen) cams -- Genera los rayos para cada pixel

          --a = map (\rayos -> listRayPhoton kdt cam figuras rayos nRay) rayitos -- Photon mapping
          a = map (\rayos -> listRayToRGB luces figuras rayos gen' nRay) rayitos -- Path tracing
          
          c = mediaLRGB a
          fin = concatMap rgbToString (gammaFunc fmx gamma c)


      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Float
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"

    _ -> do
      putStrLn "Please provide an integer as the first argument."
      exitFailure
