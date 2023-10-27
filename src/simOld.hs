{-# LANGUAGE BangPatterns #-}
import Elem3D
import Files
import Tone_map
import Figuras
import Data.List
import Data.Ord
import Data.Maybe
import Data.Bool (bool)
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

{-# INLINE obtenerPrimeraColision #-}
obtenerPrimeraColision :: [(Float, (Obj))] -> (Float,(Obj))
obtenerPrimeraColision xs = 
  case filter (\(x, _) -> x >= 0) xs of
        [] -> ((-1),(RGB 0 0 0, 0, Point3D 0 0 0, Direction 0 0 0, 0,0))
        filteredList -> (minimumBy (comparing fst) filteredList)

-- dada una lista de colisiones devuelve la lista de puntos
{-# INLINE obtenerPuntos #-}
obtenerPuntos :: [(Float, (Obj))] -> [Point3D]
obtenerPuntos lista = map (\(_, (_, _,point, _,_,_)) -> point) lista

-- dada la matriz de [figuras,colisiond e cada rayo] devuelve la lista de la primera colisión de cada rayo
{-# INLINE listRay #-}
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

calcColor :: (Float, (Obj)) -> Luz -> [Shape] -> Float -> RGB
calcColor (a, (rgb, a1, p,norm,a2,id)) luz figuras n
 | n == 0 = if colision p nxtPoint figuras then newRGB else RGB 0 0 0
 | aproxPoint nxtPoint (Point3D 0 0 0) = if colision p nxtPoint figuras then newRGB else RGB 0 0 0
 | colision p pointLuz figuras = sumRGBPoints newRGB (calcColor nxtObj luz figuras (n-1))
 | otherwise = sumRGBPoints (RGB 0 0 0) (calcColor nxtObj luz figuras (n-1))
  where
    !pointLuz = (\(Luz p _ _) -> p) luz
    !rgbLuz = (\(Luz _ rgb _) -> rgb) luz
    !intLuz = (\(Luz _ _ int) -> int) luz
    !newRGB = prodRGB newIntLuz rgbLuz rgb
    !newIntLuz = ((intLuz / ((modd (p #< pointLuz) + 1.0)**2)) * (20 / pi) *(abs ((normal norm) .* (normal (p #< pointLuz)))))
    -- dir = ranDir norm
    !figuras' = filter (\shape -> id /= getShapeID shape) figuras
    !nxtObj = obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray p norm 0)) figuras'
    !nxtPoint = (\(_, (_, _, point, _, _, _)) -> point) nxtObj
  

inYpos :: Direction  -> Point3D -> Bool
inYpos (Direction dx dy dz)  (Point3D x y z) = (dx * x + dy * y + dz * z) >= 0

{-# INLINE colision #-}
colision :: Point3D -> Point3D -> [Shape] -> Bool
colision p0 luz figuras = aproxPoint p0 bonk
  where
    !bonk = (\(_, (_, _, point, _, _, _)) -> point) $ obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray luz (normal(p0 #< luz)) 0)) figuras
        

listRayToRGB :: Luz -> Point3D -> [Shape] -> [[(Float, Obj)]] -> [RGB]
listRayToRGB luz cam figuras listaDeColisiones = luzXRayo
  where
    !rayosColisiones = listRay listaDeColisiones
    !pointLuz = (\(Luz p _ _) -> p) luz
    !luzXRayo = map (\colision -> calcColor colision luz figuras 50) rayosColisiones

pix :: Float
pix = 2000
piCam :: Float
piCam = 25
gamma = 2.6
fmx = 255
basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 (-50))
centr = Point3D (0) (10) 0
centr' = Point3D (-5) 20 0
centr'' = Point3D (10) (20) (-2)
luz = Luz (Point3D (15) (0) (0)) (RGB 255 255 255) 5
luz' = Point3D (0) (-15) (-2)
luz'' = Point3D (10) (14) (-2)
cam' =  Point3D (0) (0) (-100)
plano0 = Plane (Plano (Point3D (-20) 0 20) (Direction 1 0 0) (RGB 249 176 84) 0 0 0)
plano1 =  Plane (Plano (Point3D (20) 0 20) (Direction (1) (0) (0)) (RGB 10 10 222) 0 0 1)
plano2 =  Plane (Plano (Point3D 0 (20) 20) (Direction 0 (-1) 0) (RGB 10 255 10) 0 0 2)
plano3 =  Plane (Plano (Point3D 0 0 20) (Direction 0 0 (-1)) (RGB 171 118 24) 0 0 3)
plano4 =  Plane (Plano (Point3D 0 (-25) 20) (Direction 0 (-1) (0)) (RGB 255 0 255) 0 0 4)
plano5 =  Plane (Plano (Point3D 0 0 (-101)) (Direction 0 0 (1)) (RGB 255 255 10) 0 0 3)
bola =  Sphere (Esfera centr 5 (RGB 255 10 10) 0 0 5)
bola'' =  Sphere (Esfera centr'' 5 (RGB 155 10 155) 1 0 7)
-- tri1 = Triangle (Triangulo (Point3D 0 100 50) (Point3D 100 100 50) (Point3D 100 0 100) (RGB 60 80 100) 0 8)
camara = Camara (cam') basCam

figuras = [bola,bola'',plano0,plano1,plano2,plano3,plano4]
luces = luz
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
      let !figuras' = figuras -- ++ customTriangles ++ customTriangles1

      let rayitos = generateRaysForPixels camara pix pix --`using` parListChunk 128 rseq
      let !sol = parametricShapeCollision figuras' rayitos --`using` parListChunk 128 rseq
      traceEventIO "Principio func Luz"
      let !a = listRayToRGB luces cam' figuras' sol
      traceEventIO "Fin de so"
      let !fin = concat $ map rgbToString $ gammaFunc fmx gamma a
      -- let fin' = (gammaFunc fmx gamma) fin
      writePPM "a.ppm" (round pix) (round pix) fin
      
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Double
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"