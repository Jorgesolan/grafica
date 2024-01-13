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
      Point2D(..),
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
      chunksOf, mulCam ,brdf, sumFlLuz)
import PathTracer (pathTracer, luzDirecta, luzArea)
import KdT ( createKD )
import PhotonMap ( photonMap )
import Data.KdTree.Static ( KdTree )
import Files (writeObject)
import Debug.Trace (trace,traceEventIO)
import System.Random (StdGen, newStdGen, split)
import Data.List (transpose)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import PhotonMap (createPhoton)
import qualified Data.DList as DL
import qualified Data.Set as Set

-- make clean && make simulacion && cd ./tmp && ./simulacion && ./run.sh && cd .. && convert ./tmp/output.ppm a.bmp

-- | Función principal que le aplica antialiasing a la imagen.
{-# INLINE antialiasing #-}
antialiasing :: Int -> [Obj] -> [Obj]
antialiasing n rayos = map obtenerPrimeraColision $ map Set.fromList (chunksOf n rayos) -- Obtiene la colision mas cercana de cada lista de colisiones dependiendo del numero de rayos del antialiasing

-- | Función principal que hace el render con path tracing.
{-# INLINE listRayToRGB #-}
listRayToRGB :: [Luz] -> Set.Set Shape -> [Ray] -> StdGen -> Int -> [RGB]
listRayToRGB luz figuras rayos gen nRay = colorDirecto--zipWith (+) colorDirecto $ map (`divRGB` fromIntegral ppp) colorIndirecto
  --map (`divRGB` fromIntegral ppp) colorArea 
  where
    antial = map listRay $ parametricShapeCollision figuras rayos
    rayColisions = antialiasing nRay antial
    
    (gens, _) = splitAt (length rayColisions * ppp) $ drop 1 $ iterate (snd . split) gen  -- Semillas

    ppp = 256 -- Caminos por pixel
    colorIndirecto =zipWith (pathTracer 1 luz figuras ppp) rayColisions gens
    colorDirecto = map (luzDirecta luz figuras) rayColisions
    colorArea = zipWith (luzArea figuras ppp) rayColisions gens

-- | Función principal que hace el render con photon mapping.
{-# INLINE listRayPhoton #-}
listRayPhoton :: KdTree Float Foton -> [Luz] ->Point3D -> Set.Set Shape -> [Ray] -> Int -> [RGB]
listRayPhoton kdt luces cam figuras rayos nRay = map (photonMap kdt luces radio figuras) rayColisions
  where
    !raySMPP = map listRay $ parametricShapeCollision figuras rayos
    rayColisions = antialiasing nRay raySMPP
    radio = 3


main :: IO ()
main = do

$%$%$%$%  

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
            
      notkdt <- readObject "./kd.bin" -- Carga la lista de fotones del binario
      let kdt = createKD notkdt -- Crea el kdtree
      let cams = mulCam camara 0 0.75
      -- let cams = mulCam camara 8 0.2-- El primer número indica el número de muestras que se toman desde la cámara, el segundo el radio de apertura
          rayitos = map (\camara -> generateRaysForPixels (maxN*etapasY) etapasX n' etapaX camara (pix*aspectR) pix nRay gen) cams -- Genera los rayos para cada pixel

          a = map (\rayos -> listRayPhoton kdt luces cam figuras' rayos nRay) rayitos -- Photon mapping
          -- a = map (\rayos -> listRayToRGB luces figuras rayos gen' nRay) rayitos -- Path tracing
          
          c = mediaLRGB a
          fin = concatMap rgbToString (gammaFunc fmx gamma c)
      writePPM ("a" ++ show n ++ "_" ++ show etapaY ++ "_" ++ show etapaX ++ ".ppm") (round $ pix*aspectR) (round pix) fin

      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Float
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"
    [] -> do
      gen <- newStdGen
      start <- getCPUTime
      let !kdt = createPhoton (sumFlLuz luces) (DL.fromList []) 0 (round n) figuras' luces gen nRebotes
      print $ length kdt
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Float
      putStrLn $ "Tiempo de creacion del kdt: " ++ show diff ++ " segundos"
      writeObject "./kd.bin" kdt
    _ -> do
      putStrLn "Please provide an integer as the first argument."
      exitFailure
