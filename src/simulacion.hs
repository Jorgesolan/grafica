{-# LANGUAGE BangPatterns #-}
import Elem3D
import Files
import Figuras
import Data.List
import Data.Ord
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
import Data.List (any)
import System.CPUTime
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Control.Monad (when)
import System.Exit (exitFailure)
import System.Random

-- import Control.Parallel.Strategies
-- import qualified Data.Vector as V
-- import Control.Concurrent
-- import Control.Parallel

-- make clean && make sim && ./sim -N12 && convert a.ppm out.bmp
-- make clean && make sim && ./sim  +RTS -N -l -RTS && convert output.ppm out.bmp

-- -- Define a function to apply in parallel
-- applyFunction :: (a -> b) -> a -> MVar b -> IO ()
-- applyFunction func item resultVar = do
--     let result = func item
--     putMVar resultVar result

-- -- Apply a function to a list of items in parallel
-- parallelMap :: (a -> b) -> [a] -> IO [b]
-- parallelMap func items = do
--     !resultVars <- mapM (const newEmptyMVar) items
--     let applyFunctionWithIndex i = applyFunction func (items !! i) (resultVars !! i)
--     _ <- mapM (\i -> forkIO (applyFunctionWithIndex i)) [0..length items - 1]
--     !results <- mapM takeMVar resultVars
--     return results

-- parProc :: (a -> b) -> [a] -> [b]
-- parProc _ [] = []
-- parProc f [x] = [f x]
-- parProc f (x:xs) = par n1 (n2 `pseq` (n1 : n2))
--   where
--     n1 = f x
--     n2 = parProc f xs
{-# INLINE build #-}
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

tuplasAleatorias :: [(Float, Float)] -> Float  -> [(Float, Float)]
tuplasAleatorias inputTuplas salto = tuplasConRandoms
  where
    !randomNumbers = take (length inputTuplas * 2) $ randomRs (0.0, salto) (mkStdGen 42) :: [Float]
    -- randomNumbers = take (length inputTuplas * 2) [(-20000) ..(20000)] :: [Float]
    !tuplasConRandoms = [(x + r1, y + r2) | ((x, y), r1, r2) <- zip3 inputTuplas (take halfLen randomNumbers) (drop halfLen randomNumbers)]
    halfLen = length randomNumbers `div` 2

{-# INLINE generarTuplas #-}
generarTuplas :: [Float] -> [Float] -> [(Float, Float)]
generarTuplas xs ys = [(x, y) | y <- ys, x <- xs]

generateRaysForPixels :: Int -> Int -> Camara -> Float -> Float -> Float  -> [Ray]
generateRaysForPixels maxN n (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height j =
  map (\(x, y) -> Ray p (generateDirection x y focal) 10) tuplasRandom
  where
      piY = py / height
      piX = px / width
      px' = px / 2
      py' = py / 2
      !yValues = [(-py'), (piY-py') .. (py'-piY)]
      yCount = length yValues
      yStep = yCount `div` maxN
      startIdx = (n - 1) * yStep
      endIdx = n * yStep
      !selectedYValues = take (endIdx - startIdx) (drop startIdx yValues)
      generateDirection width height focal = normal ((Point3D width height focal) #< p)
      !tuplas = generarTuplas  (concatMap (replicate (round j))[(-px'), (piX-px') ..(px'-piX)])  selectedYValues
      !tuplasRandom = tuplasAleatorias tuplas piY

{-# INLINE obtenerPrimeraColision #-}
obtenerPrimeraColision :: [(Float, (Obj))] -> (Float,(Obj))
obtenerPrimeraColision xs = 
  case filter (\(x, _) -> x >= 0) xs of
        [] -> (0,(RGB 0 0 0, 0, Point3D 0 0 0, Direction 0 0 0, 0,0))
        filteredList -> (minimumBy (comparing fst) filteredList)

-- dada una lista de colisiones devuelve la lista de puntos
{-# INLINE obtenerPuntos #-}
obtenerPuntos :: [(Float, (Obj))] -> [Point3D]
obtenerPuntos lista = map (\(_, (_, _,point, _,_,_)) -> point) lista

-- dada la matriz de [figuras,colisiond e cada rayo] devuelve la lista de la primera colisión de cada rayo
{-# INLINE listRay #-}
listRay :: [[(Float, (Obj))]] -> [(Float, (Obj))]
listRay = map obtenerPrimeraColision . transpose

{-# INLINE calcularDirESpejo #-}
calcularDirESpejo :: Direction -> Direction -> Direction
calcularDirESpejo d normal = d - (escalateDir (2 * (d .* normal)) normal)

sumRGB :: (Float, (Obj)) -> (Float, (Obj)) -> (Float, (Obj))
sumRGB (f, (RGB r g b, fl, p, d, lum, id)) (_, (RGB r' g' b', _, _, _, _, _)) = (f, (RGB (r + r') (g + g') (b + b'), fl, p, d, lum, id))

mediaRGB :: Float -> [(Float, (Obj))] -> (Float, (Obj))
mediaRGB n lista = medRGB (1/n) $ foldr sumRGB (head lista) (tail lista)
  where
    medRGB n (f, (RGB r g b, fl, p, d, lum, id)) = (f, (RGB ( r * n) ( g * n) ( b * n), fl, p, d, lum, id))

-- mediaDeRayos :: [[(Float, (Obj))]] -> [(Float, (Obj))]
-- mediaDeRayos =  map (\rayos -> mediaRGB rayos (fromIntegral(length rayos))) . transpose

searchLight :: Obj -> Ray -> [Shape] -> RGB
searchLight obj@(rgb, _, pFig, normal, lum, id) ray@(Ray pRay dir n) figuras
  | meta ray figuras || id == 8 = rgb
  | n == 0 = RGB 0 0 0
  | otherwise = searchLight bank ray' figuras
  where
    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    !bonk = snd $ obtenerPrimeraColision $ map (\shape -> oneCollision shape ray') figuras'
    rgbonk = (\ (rgb, _, _, _, _, _) -> rgb) bonk
    !bank = fldsmdfr bonk $ rgbMedio rgbonk rgb
    ray' = Ray pFig (calcularDirESpejo (pFig #< pRay) normal) (n - 1)

    meta ray' figuras' = lum > 0
      where
        (_, (_, _, _, _, lum', _)) = obtenerPrimeraColision $ map (\shape -> oneCollision shape ray') figuras'

    rgbMedio (RGB r0 b0 g0) (RGB r1 b1 g1) = RGB ((r0 + r1) / 2) ((b0 + b1) / 2) ((g0 + g1) / 2)
    fldsmdfr (_, f, p, d, l, id') r = (r, f, p, d, l, id')

{-# INLINE antialiasing #-}
antialiasing :: [(Float, (Obj))] -> Float-> [(Float, (Obj))]
antialiasing rayos n =  map (mediaRGB (n)) (chunksOf (round (n)) rayos)


pathTracer :: Point3D -> [Shape] -> [[(Float, (Obj))]] -> Float -> [RGB]
pathTracer cam figuras listaDeColisiones n = b
  where
    !rayosParser = map (\colision -> antialiasing colision n) listaDeColisiones
    !rayosColisiones = listRay rayosParser
    -- !rayosColisiones = (listRay listaDeColisiones)

    !b = map (search cam figuras) rayosColisiones
      where
        search cam shapes esp@(f, obj@(_, _, p, _, _, _)) = searchLight obj (Ray cam (p #< cam) 10) shapes

    -- !b = map (\(_, (rgb, _,_, _,_,_)) -> rgb) $ map (oneEspejo cam figuras) rayosColisiones
    --   where
    --    oneEspejo cam shapes esp@(f, (rgb, ref, p, d, lum, id))
    --       | ref == 0 = esp
    --       | otherwise = let
    --           otherShapes = filter (\shape -> not (id == getShapeID shape)) shapes
    --           cortes = map (\shape -> oneCollision shape (Ray p (calcularDirESpejo (p #< cam) d) 0)) otherShapes
    --           rgbRefle = (\(_, (rgb, _, _, _, _, _)) -> rgb) $ obtenerPrimeraColision cortes
    --           !newRgb = if (ref < 0.5) then rgb else rgbRefle
    --         in (f, (newRgb, ref, p, d, lum, id))
            
    -- traceEventIO "Fin Func"
    -- `using` parList rseq
    -- parMap rdeepseq

pix :: Float
pix = 1080
maxN = 1
etapas = 1
nRay :: Float
nRay = 20


piCam :: Float
piCam = 25
basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 (-50))
centr = Point3D (0) (10) 0
centr' = Point3D (-5) 20 0
centr'' = Point3D (10) (20) (-2)
luz = Point3D (15) (0) (-2)
luz' = Point3D (0) (-15) (-2)
luz'' = Point3D (10) (14) (-2)
cam' =  Point3D (0) (0) (-100)
plano0 = Plane (Plano (Point3D (-20) 0 0) (Direction 1 0 0) (RGB 249 176 84) 0 0 0)
plano1 =  Plane (Plano (Point3D (20) 0 0) (Direction (-1) (0) (0)) (RGB 146 223 222) 0 0 1)
plano2 =  Plane (Plano (Point3D 0 (20) 0) (Direction 0 (-1) 0) (RGB 0 255 0) 0 0 2)
plano3 =  Plane (Plano (Point3D 0 0 20) (Direction 0 0 (-1)) (RGB 171 118 24) 0 0 3)
plano4 =  Plane (Plano (Point3D 0 (-25) 0) (Direction 0 (1) (0)) (RGB 255 255 255) 0 1 4)
plano5 =  Plane (Plano (Point3D 0 0 (-101)) (Direction 0 0 (1)) (RGB 255 255 0) 0 0 3)
bola =  Sphere (Esfera centr 5 (RGB 255 0 0) 0 0 5)
bola'' =  Sphere (Esfera centr'' 5 (RGB 155 0 155) 1 0 7)
area = Sphere (Esfera (Point3D 0 0 0) 100 (RGB 255 255 255) 0 1 8)
-- tri1 = Triangle (Triangulo (Point3D 0 100 50) (Point3D 100 100 50) (Point3D 100 0 100) (RGB 60 80 100) 0 8)
camara = Camara (cam') basCam

figuras = [bola,bola'',plano0,plano1,plano2,plano3,plano4,area]
luces = [luz,luz',luz'']
-- figurasSinPlanos = (parametricShapeCollision [bola,bola',bola''])

main :: IO ()
main = do

      args <- getArgs
      case listToMaybe args of
        Just nStr -> do
            let n = read nStr :: Int
            -- The rest of your program here, using the 'n' value as needed.
            -- For example, you can pass 'n' to the 'generateRaysForPixels' function.
            putStrLn $ "The value of 'n' is: " ++ show n
            -- Your other code here.
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

            let rayitos = generateRaysForPixels (maxN*etapas) n camara pix pix nRay --`using` parListChunk 128 rseq
            let !sol = parametricShapeCollision figuras' rayitos --`using` parListChunk 128 rseq
            traceEventIO "Principio func Luz"
            let !a = pathTracer cam' figuras' sol nRay
            traceEventIO "Fin de so"
            let !fin = concat $ map rgbToString a
            
            writePPM ("a" ++ (show n) ++ ".ppm") (round pix) (round pix) fin
            
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10^12) :: Double
            putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

            traceEventIO "END"
        Nothing -> do
            putStrLn "Please provide an integer as the first argument."
            exitFailure