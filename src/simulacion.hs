{-# LANGUAGE BangPatterns #-}
import Elem3D
import Files
import Figuras
import Data.List
import Data.Ord
import Data.Maybe
-- import Control.Parallel.Strategies
import Debug.Trace
import Data.List (any)
-- import System.Random
import System.CPUTime
-- import qualified Data.Vector as V
-- import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
-- import Control.Parallel

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Control.Monad (when)
import System.Exit (exitFailure)

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

generateRaysForPixels :: Int -> Int -> Camara -> Float -> Float -> [Ray]
generateRaysForPixels maxN n (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height =
  [Ray p (generateDirection x y focal) 10 | y <- selectedYValues, x <- ([(-px'), (piX-px') ..(px'-piX)])]
  where
    !piY = py / height
    !piX = px / width
    px' = px / 2
    py' = py / 2
    yValues = [(-py'), (piY-py') .. (py'-piY)]
    yCount = length yValues
    yStep = yCount `div` maxN
    startIdx = (n - 1) * yStep
    endIdx = n * yStep
    selectedYValues = take (endIdx - startIdx) (drop startIdx yValues)
    generateDirection width height focal = normal ((Point3D width height focal) #< p)


-- generateRaysForPixels :: Int -> Int -> Camara -> Float -> Float -> [Ray]
-- generateRaysForPixels maxN n (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height =
--   [Ray p (generateDirection x y focal) 10 | y <- ({- zipWith (+) randomY -} [(-py'), (piY-py') ..(py'-piY)]), x <- ({- zipWith (+) randomX -} [(-px'), (piX-px') ..(px'-piX)])]
--   where
--       !piY = py / height
--       !piX = px / width
--       px' = px / 2
--       py' = py / 2
--       generateDirection width height focal = normal ((Point3D width height focal) #< p)

      -- no hace falta
      -- gen = mkStdGen 42
      -- randomY = take (round(height)) $ randomRs (0.0, (piY-py')) gen :: [Float]
      -- randomX = take (round(width)) $ randomRs (0.0, (piY-py')) gen :: [Float]


-- obtenerPrimeraColision :: [(Float, (Obj))] -> (Float,(Obj))
-- obtenerPrimeraColision = minimumBy (comparing fst) . filter (\(x, _) -> x >= 0)

obtenerPrimeraColision :: [(Float, (Obj))] -> (Float,(Obj))
obtenerPrimeraColision xs = 
  case filter (\(x, _) -> x >= 0) xs of
        [] -> (0,(RGB 0 0 0, 0, Point3D 0 0 0, Direction 0 0 0, 0,0))
        filteredList -> (minimumBy (comparing fst) filteredList)

-- dada una lista de colisiones devuelve la lista de puntos
obtenerPuntos :: [(Float, (Obj))] -> [Point3D]
obtenerPuntos lista = map (\(_, (_, _,point, _,_,_)) -> point) lista

-- dada la matriz de [figuras,colisiond e cada rayo] devuelve la lista de la primera colisión de cada rayo
listRay :: [[(Float, (Obj))]] -> [(Float, (Obj))]
listRay = map obtenerPrimeraColision . transpose

calcularDirESpejo :: Direction -> Direction -> Direction
calcularDirESpejo d normal = d - (escalateDir (2 * (d .* normal)) normal)

sumRGB :: (Float, (Obj)) -> (Float, (Obj)) -> (Float, (Obj))
sumRGB (f, (RGB r g b, fl, p, d, lum, id)) (_, (RGB r' g' b', _, _, _, _, _)) = (f, (RGB (r + r') (g + g') (b + b'), fl, p, d, lum, id))

mediaRGB :: [(Float, (Obj))] -> Float -> (Float, (Obj))
mediaRGB lista n = medRGB (1/n) $ foldr sumRGB (head lista) (tail lista)
  where
    medRGB n (f, (RGB r g b, fl, p, d, lum, id)) = (f, (RGB ( r * n) ( g * n) ( b * n), fl, p, d, lum, id))

mediaDeRayos :: [[(Float, (Obj))]] -> [(Float, (Obj))]
mediaDeRayos =  map (\rayos -> mediaRGB rayos (fromIntegral(length rayos))) . transpose


searchLight :: Obj -> Ray -> [Shape] -> RGB
searchLight obj@(rgb,_,pFig,normal,lum,id) ray@(Ray pRay dir n) figuras 
  | meta ray figuras = rgb 
  | id == 8 = rgb
  | n == 0 = {-rgbMedio (rgb) (rgbonk) -} RGB 0 0 0
  | otherwise = searchLight bank ray' figuras
  where
    !figuras' = filter (\shape -> not (id == getShapeID shape)) figuras
    !bonk = snd . obtenerPrimeraColision $ map (\shape -> oneCollision shape ray') figuras'
    fldsmdfr (_,f,p,d,l,id) r = (r,f,p,d,l,id)
    bank = fldsmdfr bonk (rgbMedio ((\(rgb, _, _, _, _, _) -> rgb) bonk) rgb)
    -- d = (\(_, (_, _, _, dir, _, _)) -> dir) bonk
    -- p' = (\(_, (_, _, point, _, _, _)) -> point) bonk
    -- rgbonk = (\(_, (rgb, _, _, _, _, _)) -> rgb) bonk
    -- idbonk = (\(_, (_, _, _, _, _, id)) -> id) bonk
    -- !rgb' = rgbMedio (rgb) (rgbonk)
    !ray' = Ray pFig (calcularDirESpejo (pFig #< pRay) (normal)) (n-1)
    meta ray figuras = lum > 0
      where
        (_, (_, _, _, _, lum, _)) = obtenerPrimeraColision $ map (\shape -> oneCollision shape ray) figuras
        --NO lanzar rayo de rebote en busca de la luz contra el objeto del que parte dicho rqayo!!!!
    rgbMedio (RGB r0 b0 g0) (RGB r1 b1 g1) = RGB ((r0 + r1)/2) ((b0 + b1)/2) ((g0 + g1)/2)
    -- Primera vez ya sabes el objeto que chocas, tienes su punto colision, 
    -- lanzas de ahi el rayo a su dir espejo y obtienes el objeto q chocas, repites


pathTracer :: Point3D -> [Shape] -> [[(Float, (Obj))]] -> [RGB]
pathTracer cam figuras listaDeColisiones = b
  where
    !rayosColisiones = listRay listaDeColisiones
    

    !b = map (search cam figuras) rayosColisiones
      where
        search cam shapes esp@(f, obj@(_, _, p, _, _, _)) = result
          where
            !newRgb = searchLight obj (Ray cam (p #< cam) 10) shapes
            result = newRgb

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
pix = 4096
maxN = 32
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

            let rayitos = generateRaysForPixels maxN n camara pix pix --`using` parListChunk 128 rseq
            let !sol = parametricShapeCollision figuras' rayitos --`using` parListChunk 128 rseq
            traceEventIO "Principio func Luz"
            let !a = pathTracer cam' figuras' sol
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