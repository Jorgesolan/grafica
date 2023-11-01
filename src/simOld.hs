{-# LANGUAGE BangPatterns #-}
import Elem3D
import Files
import Tone_map
import Figuras

import Data.Ord (comparing)
import Debug.Trace (trace,traceEventIO)
import Data.List (any,minimumBy,transpose)
import System.Random (randomR, StdGen, newStdGen, randomRs, split)
import System.IO.Unsafe (unsafePerformIO)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)
-- make clean && make sim && ./sim -N12 && convert a.ppm out.bmp
-- make clean && make sim && ./sim  +RTS -N -l -RTS && convert a.ppm out.bmp
-- make clean && make simOld && cd ./tmp && ./run.sh && cd ..


{-# INLINE build #-}
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

--Divide una lista en n sublistas
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

{-# INLINE polarToCartesian #-}
polarToCartesian :: Float -> Float -> Point3D
polarToCartesian inclinacion azimut = Point3D (sin inclinacion * cos azimut) (cos inclinacion) (sin inclinacion * sin azimut)

{-# INLINE genPoint #-}
genPoint :: StdGen -> StdGen -> Point3D
genPoint gen1 gen2 = polarToCartesian (acos $ sqrt 1 - randIncl) (2 * pi * randAz)
  where
   !(randIncl, _) = randomR (0.0, 1.0) gen1 :: (Float, StdGen)
   !(randAz, _) = randomR (0.0, 1.0) gen2 :: (Float, StdGen)

tuplasAleatorias :: [(Float, Float)] -> Float -> StdGen  -> [(Float, Float)]
tuplasAleatorias inputTuplas salto gen = tuplasConRandoms
  where
    !randomNumbers = take (length inputTuplas * 2) $ randomRs (0.0, salto) gen :: [Float]
    !tuplasConRandoms = [(x + r1, y + r2) | ((x, y), r1, r2) <- zip3 inputTuplas (take halfLen randomNumbers) (drop halfLen randomNumbers)]
    !halfLen = length randomNumbers `div` 2

{-# INLINE generarTuplas #-}
generarTuplas :: [Float] -> [Float] -> [(Float, Float)]
generarTuplas xs ys = [(x, y) | y <- ys, x <- xs]

--Genera los rayos para cada pixel
generateRaysForPixels :: Int -> Int -> Camara -> Float -> Float -> Float -> StdGen -> [Ray]
generateRaysForPixels maxN n (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height j gen =
  map (\(x, y) -> Ray p (generateDirection x y focal) 10) tuplasRandom
  where
    piY = py / height
    piX = px / width
    px' = px / 2
    py' = py / 2
    !yValues = [(-py'), (piY-py') .. (py'-piY)]
    yStep = (length yValues) `div` maxN
    startIdx = (n - 1) * yStep
    endIdx = n * yStep
    !selectedYValues = take (endIdx - startIdx) (drop startIdx yValues)
    generateDirection width height focal = normal $ (Point3D width height focal) #< p
    !tuplas = generarTuplas  (concatMap (replicate (round j))[(-px'), (piX-px') ..(px'-piX)]) selectedYValues
    !tuplasRandom = tuplasAleatorias tuplas piY gen

--Devuelve la primera colision de cada lista de colisiones
{-# INLINE obtenerPrimeraColision #-}
obtenerPrimeraColision :: [(Float, Obj)] -> (Float,Obj)
obtenerPrimeraColision xs = 
  case filter (\(x, _) -> x >= 0) xs of
    [] -> ((-1),(RGB 0 0 0, 0, Point3D 0 0 0, Direction 0 0 0, 0,0))
    filteredList -> (minimumBy (comparing fst) filteredList)

-- dada una lista de colisiones devuelve la lista de puntos
{-# INLINE obtenerPuntos #-}
obtenerPuntos :: [(Float, Obj)] -> [Point3D]
obtenerPuntos lista = map (\(_, (_, _,point, _,_,_)) -> point) lista

{-# INLINE obtenerPunto #-}
obtenerPunto :: (Float, Obj) -> Point3D
obtenerPunto (_, (_, _,point, _,_,_)) = point

-- dada la lista de listas de colisiones, devuelve la lista de la primera colisión de cada rayo
{-# INLINE listRay #-}
listRay :: [[(Float, Obj)]] -> [(Float, Obj)]
listRay = map obtenerPrimeraColision . transpose

calcularDirESpejo :: Direction -> Direction -> Direction
calcularDirESpejo d normal = d - (escalateDir (2 * (d .* normal)) normal)

calcColor :: (Float, Obj) -> Luz -> [Shape] -> StdGen -> Float -> RGB
calcColor (a, (rgb, a1, p,norm,a2,id)) (Luz pointLuz rgbLuz intLuz) figuras gen n
  | n == 0 || aproxPoint nxtPoint (Point3D 0 0 0) = if colision p pointLuz figuras then newRGB else RGB 0 0 0 -- Si fin o choca con "Infinito" cambiar eso
  | colision p pointLuz figuras = {- sumRGBPoints newRGB $ -} calcColor nxtObj luz figuras gen' (n-1) -- Si llega luz a ese punto, no choca con otras figuras antes
  | otherwise = {- sumRGBPoints (RGB 0 0 0) $ -} calcColor nxtObj luz figuras gen' (n-1) -- Si no llega luz pq antes choca con otras figuras
  where
    !(gen',gen'') = split gen
    !newRGB =  prodRGB newIntLuz rgbLuz rgb -- Modificar luz depende del color que le llega
    !figuras' = filter (\shape -> id /= getShapeID shape) figuras
    !nxtObj = obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray p (normal(puntoAl #< p)) 0)) figuras' -- Siguiente objeto que choca, q no sea el mismo
    !nxtPoint = obtenerPunto nxtObj

    !newIntLuz = (intLuz / ((modd (p #< pointLuz) + 1.0)**2)) * (20 / pi) * (abs (norm .* (normal (p #< pointLuz)))) -- "Integral"
    !puntoAl = cambioBase' p (generateBase algxa norm (normal (algxa * norm))) (genPoint gen gen'')
    !algxa = normal $ norm * Direction 2 1 (-2) -- Direccion cualquiera para que no se repita, si peta cambiar esto

{-# INLINE colision #-}
colision :: Point3D -> Point3D -> [Shape] -> Bool
colision p0 luz figuras = aproxPoint p0 bonk -- Si es el mismo punto, no choca con nada
  where
    !bonk = obtenerPunto $ obtenerPrimeraColision $ map (\figura -> oneCollision figura ray) figuras --Saca el punto de la primera colision de la luz con las figuras
    !ray = Ray luz (normal $ p0 #< luz) 0
    
        
{-# INLINE antialiasing #-}
antialiasing :: [(Float, Obj)] -> Float-> [(Float, Obj)]
antialiasing rayos n =  map (obtenerPrimeraColision) (chunksOf (round n) rayos) -- Obtiene la colision mas cercana de cada lista de colisiones dependiendo del numero de rayos del antialiasing

listRayToRGB :: Luz -> Point3D -> [Shape] -> [Ray] -> StdGen -> Float -> [RGB]
listRayToRGB luz cam figuras rayos gen n = luzXRayo
  where
    !(gens, gen') = splitAt (length rayos) $ tail $ iterate (snd . split) gen
    !luzXRayo = zipWith (\(colision, gen'') rayo -> calcColor colision luz figuras gen'' 1) (zip (listRay $ map (\colision -> antialiasing colision n) $ parametricShapeCollision figuras rayos) gens) rayos

pix :: Float
pix = 500
maxN = 8
etapas = 4
nRay = 1
piCam :: Float
piCam = 25
gamma = 2.8
fmx = 255
basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 (-50))
centr = Point3D (0) (10) 0
centr' = Point3D (0) 0 0
centr'' = Point3D (10) (20) (-2)
luz = Luz (Point3D (15) (0) (0)) (RGB 255 255 255) 5
luz' = Point3D (0) (-15) (-2)
luz'' = Point3D (10) (14) (-2)
cam' =  Point3D (0) (0) (-100)
plano0 = Plane (Plano (Point3D (-20) 0 0) (Direction 1 0 0) (RGB 249 176 84) 0 1 0)
plano1 =  Plane (Plano (Point3D (20) 0 0) (Direction (-1) (0) (0)) (RGB 10 10 222) 0 0 1)
plano2 =  Plane (Plano (Point3D 0 (20) 0) (Direction 0 (-1) 0) (RGB 10 255 10) 0 0 2)
plano3 =  Plane (Plano (Point3D 0 0 20) (Direction 0 0 (-1)) (RGB 171 118 24) 0 0 3)
plano4 =  Plane (Plano (Point3D 0 (-25) 0) (Direction 0 (1) (0)) (RGB 255 0 255) 0 0 4)
plano5 =  Plane (Plano (Point3D 0 0 (-101)) (Direction 0 0 (1)) (RGB 255 255 10) 0 0 5)
bola =  Sphere (Esfera centr 5 (RGB 255 10 10) 0 0 6)
bola' =  Sphere (Esfera centr'' 5 (RGB 255 255 255) 1 0 7)
donut = Donut (Rosquilla centr' (Direction 0 0 (-1)) 20 3 (RGB 10 255 255) 0 0 8)
-- tri1 = Triangle (Triangulo (Point3D 0 100 50) (Point3D 100 100 50) (Point3D 100 0 100) (RGB 60 80 100) 0 8)
camara = Camara (cam') basCam

figuras = [bola,bola',plano0,plano1,plano2,plano3,plano4,plano5]
luces = luz
-- figurasSinPlanos = (parametricShapeCollision [bola,bola',bola''])
main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Just nStr -> do
      gen <- newStdGen
      gen' <- newStdGen
      let n = read nStr :: Int
      
      putStrLn $ "The value of 'n' is: " ++ show n
      traceEventIO "START"
      start <- getCPUTime 
      let objFilePath = "diamante.obj"  
      (vertices, triangles) <- loadObjFile objFilePath
      let !vertices' = map (rotatePoint 'X' (degToRad 0).movePoint (Direction (-5) (-2.5) 0).escalatePoint (2.5)) vertices
      let !customTriangles = convertToCustomFormat (vertices', triangles)

      let objFilePath1 = "cubo.obj"  
      (vertices1, triangles1) <- loadObjFile objFilePath1
      let !vertices1' = map (rotatePoint 'X' (0).movePoint (Direction (-5) (0) 0).escalatePoint (2.5)) vertices1
      let !customTriangles1 = convertToCustomFormat (vertices1', triangles1)
      let !figuras' = figuras -- ++ customTriangles ++ customTriangles1

      let rayitos = generateRaysForPixels (maxN*etapas) n camara pix pix nRay gen 
      traceEventIO "Principio func Luz"
      let !a = listRayToRGB luces cam' figuras' rayitos gen' nRay
      traceEventIO "Fin de so"
      let !fin = concat $ map rgbToString $ gammaFunc fmx gamma a
      
      writePPM ("a" ++ (show n) ++ ".ppm") (round pix) (round pix) fin
      
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Double
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"

      traceEventIO "END"

    Nothing -> do
      putStrLn "Please provide an integer as the first argument."
      exitFailure