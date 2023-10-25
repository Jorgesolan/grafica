{-# LANGUAGE BangPatterns #-}
import Elem3D
import Files
import Tone_map
import Figuras
import Data.List
import Data.Ord
import Data.Maybe
import Control.Parallel.Strategies
import Debug.Trace
import Data.List (any)
import System.Random
import System.CPUTime
import qualified Data.Vector as V
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Control.Parallel
-- make clean && make sim && ./sim -N12 && convert a.ppm out.bmp
-- make clean && make sim && ./sim  +RTS -N -l -RTS && convert a.ppm out.bmp

-- Define a function to apply in parallel
applyFunction :: (a -> b) -> a -> MVar b -> IO ()
applyFunction func item resultVar = do
    let result = func item
    putMVar resultVar result

-- Apply a function to a list of items in parallel
parallelMap :: (a -> b) -> [a] -> IO [b]
parallelMap func items = do
    !resultVars <- mapM (const newEmptyMVar) items
    let applyFunctionWithIndex i = applyFunction func (items !! i) (resultVars !! i)
    _ <- mapM (\i -> forkIO (applyFunctionWithIndex i)) [0..length items - 1]
    !results <- mapM takeMVar resultVars
    return results

parProc :: (a -> b) -> [a] -> [b]
parProc _ [] = []
parProc f [x] = [f x]
parProc f (x:xs) = par n1 (n2 `pseq` (n1 : n2))
  where
    n1 = f x
    n2 = parProc f xs

generateRaysForPixels :: Camara -> Float -> Float -> [Ray]
generateRaysForPixels (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height =
  [Ray p (generateDirection x y focal) 10 | y <- ({- zipWith (+) randomY -} [(-py'), (piY-py') ..(py'-piY)]), x <- ({- zipWith (+) randomX -} [(-px'), (piX-px') ..(px'-piX)])]
  where
      !piY = py / height
      !piX = px / width
      px' = px / 2
      py' = py / 2
      generateDirection width height focal = normal ((Point3D width height focal) #< p)
      gen = mkStdGen 42
      randomY = take (round(height)) $ randomRs (0.0, (piY-py')) gen :: [Float]
      randomX = take (round(width)) $ randomRs (0.0, (piY-py')) gen :: [Float]

obtenerPrimeraColision :: [(Float, (Obj))] -> (Float,(Obj))
obtenerPrimeraColision = minimumBy (comparing fst) . filter (\(x, _) -> x >= 0)

-- dada una lista de colisiones devuelve la lista de puntos
obtenerPuntos :: [(Float, (Obj))] -> [Point3D]
obtenerPuntos lista = map (\(_, (_, _,point, _,_,_)) -> point) lista

-- dada la matriz de [figuras,colisiond e cada rayo] devuelve la lista de la primera colisión de cada rayo
listRay :: [[(Float, (Obj))]] -> [(Float, (Obj))]
listRay = map obtenerPrimeraColision . transpose

calcularDirESpejo :: Direction -> Direction -> Direction
calcularDirESpejo d normal = d - (escalateDir (2 * (d .* normal)) normal)

{-# INLINE sumRGB #-}
sumRGB :: (Float, (Obj)) -> (Float, (Obj)) -> (Float, (Obj))
sumRGB (f, (RGB r g b, fl, p, d,flo, id)) (_, (RGB r' g' b', _, _, _, _, _)) = (f, (RGB (r + r') (g + g') (b + b'), fl, p, d, flo,id))

{-# INLINE mediaRGB #-}
mediaRGB :: [(Float, (Obj))] -> Float -> (Float, (Obj))
mediaRGB lista n = medRGB (1/n) $ foldr sumRGB (head lista) (tail lista)
  where
    medRGB n (f, (RGB r g b, fl, p, d,flo, id)) = (f, (RGB ( r * n) ( g * n) ( b * n), fl, p, d,flo, id))

mediaDeRayos :: [[(Float, (Obj))]] -> [(Float, (Obj))]
mediaDeRayos =  map (\rayos -> mediaRGB rayos (fromIntegral(length rayos))) . transpose

calcColor :: (Float, (Obj)) -> [Point3D] -> (Float, (Obj))
calcColor obj [] = obj
calcColor obj (x:xs) = (a,(newRGB,a1,p,norm,a2,a3))
  where
    (a, (rgb@(RGB r g b), a1, p,norm,a2,a3)) = obj
    newRGB = prodRGB ((5.0 / ((modd (p #< x) + 1.0)**2)) * (20 / pi) *(abs ((normal norm) .* (normal (p #< x))))) rgb
    
listRayToRGB :: [Point3D] -> Point3D -> [Shape] -> [[(Float, (Obj))]] -> [(Float, (Obj))]
listRayToRGB luces cam figuras listaDeColisiones = mediaDeRayos luzXRayo
  where
    !parametricfiguras = parametricShapeCollision figuras
    !rayosColisiones = listRay listaDeColisiones

    !ligthRays = [[Ray luz (punto #< luz) 0 | punto <- obtenerPuntos rayosColisiones] | luz <- luces]
    -- !collisions = unsafePerformIO $ parallelMap parametricfiguras ligthRays
    !collisions = map parametricfiguras ligthRays

    !luzXRayo = [zipWith eligeResultado rayosColisiones (listRay collision) | collision <- collisions]
      where
        eligeResultado a@(t, ((RGB r0 g0 b0), ra, pa, d,f, id)) (_, ((RGB r1 g1 b1), _, pb, _,_, _))
          | aproxPoint pa pb = calcColor a luces
          | otherwise = (t, (RGB 0 0 0, ra, pa, d,f, id))


pix :: Float
pix = 1080
piCam :: Float
piCam = 25
gamma = 2.4
fmx = 255
basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 (-50))
centr = Point3D (0) (10) 0
centr' = Point3D (-5) 20 0
centr'' = Point3D (10) (20) (-2)
luz = Point3D (0) (-20) (0)
luz' = Point3D (0) (-15) (-2)
luz'' = Point3D (10) (14) (-2)
cam' =  Point3D (0) (0) (-100)
plano0 = Plane (Plano (Point3D (-20) 0 20) (Direction 1 0 0) (RGB 249 176 84) 0 0 0)
plano1 =  Plane (Plano (Point3D (20) 0 20) (Direction (1) (0) (0)) (RGB 146 223 222) 0 0 1)
plano2 =  Plane (Plano (Point3D 0 (20) 20) (Direction 0 (-1) 0) (RGB 0 255 0) 0 0 2)
plano3 =  Plane (Plano (Point3D 0 0 20) (Direction 0 0 (-1)) (RGB 171 118 24) 0 0 3)
plano4 =  Plane (Plano (Point3D 0 (-25) 20) (Direction 0 (-1) (0)) (RGB 255 0 255) 0 0 4)
plano5 =  Plane (Plano (Point3D 0 0 (-101)) (Direction 0 0 (1)) (RGB 255 255 0) 0 0 3)
bola =  Sphere (Esfera centr 5 (RGB 255 0 0) 0 0 5)
bola'' =  Sphere (Esfera centr'' 5 (RGB 155 0 155) 1 0 7)
-- tri1 = Triangle (Triangulo (Point3D 0 100 50) (Point3D 100 100 50) (Point3D 100 0 100) (RGB 60 80 100) 0 8)
camara = Camara (cam') basCam

figuras = [bola,bola'',plano0,plano1,plano2,plano3,plano4]
luces = [luz]
-- figurasSinPlanos = (parametricShapeCollision [bola,bola',bola''])
main :: IO ()
main = do
      traceEventIO "START"
      start <- getCPUTime 
      let objFilePath = "diamante.obj"  -- Replace with the path to your .obj file
      (vertices, triangles) <- loadObjFile objFilePath
      let !vertices' = map (rotatePoint 'Y' (0).movePoint (Direction (-5) (-2.5) 0).escalatePoint (2.5)) vertices
      let !customTriangles = convertToCustomFormat (vertices', triangles)

      let objFilePath1 = "cubo.obj"  -- Replace with the path to your .obj file
      (vertices1, triangles1) <- loadObjFile objFilePath1
      let !vertices1' = map (rotatePoint 'X' (0).movePoint (Direction (-5) (0) 0).escalatePoint (2.5)) vertices1
      let !customTriangles1 = convertToCustomFormat (vertices1', triangles1)
      let !figuras' = figuras ++ customTriangles ++ customTriangles1

      let rayitos = generateRaysForPixels camara pix pix --`using` parListChunk 128 rseq
      let !sol = parametricShapeCollision figuras' rayitos --`using` parListChunk 128 rseq
      traceEventIO "Principio func Luz"
      let !a = listRayToRGB luces cam' figuras' sol
      traceEventIO "Fin de so"
      let !fin = concat $ map rgbToString . (gammaFunc fmx gamma) . map (\(_, (rgb, _, _, _,_, _)) -> rgb) $ a
      -- let fin' = (gammaFunc fmx gamma) fin
      writePPM "a.ppm" (round pix) (round pix) fin
      
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Double
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"