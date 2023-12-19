{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Funciones where

import Codec.Picture
import Elem3D
    ( RGB(..),
      Luz(..),
      Base(Base),
      Ray(..),
      Direction(..),
      Point3D(..),
      (#<),
      aproxPoint,
      (.*),
      escalateDir,
      modd,
      normal,
      modRGB,
      divRGB,
      scale,
      generateBase,
      cambioBase, addPoints, pointDir, Foton, distFot, (#), escalatePoint, distPoint )
import Figuras
    ( oneCollision,
      Camara(..),
      Obj(..),
      Rectangulo(Rectangulo),
      Shape(Rectangle) )

import System.IO.Unsafe (unsafePerformIO)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Data.List (minimumBy,transpose)
import System.Random (randomR, StdGen, randomRs,split, mkStdGen)

import Data.Either (fromRight)
import Data.Binary (Word8)
--------------------------
-- FUNCIONES DE OBJETOS --
--------------------------

{-# INLINE objAleatorio #-}
objAleatorio :: [Shape] -> Obj -> StdGen -> StdGen -> Obj
objAleatorio figuras obj gen gen' = nxtObj
  where
    !nxtObj = snd $ obtenerPrimeraColision $ map (oneCollision (Ray (colObj obj) $ normal (puntoAl #< colObj obj))) figuras -- Siguiente objeto que choca, q no sea el mismo
    puntoAl = cambioBase (colObj obj) (generateBase dirAl (normObj obj) (normal (dirAl * normObj obj))) $ genPoint gen gen'
    dirAl = normal $ normObj obj * Direction 2 1 (-2) -- Direccion cualquiera para que no se repita, si peta cambiar esto

{-# INLINE objEspejo #-}
objEspejo :: [Shape] -> Direction -> Direction -> Point3D -> Obj
objEspejo figuras w0 normal p = snd $ obtenerPrimeraColision $ map (oneCollision (Ray p newDir)) figuras
  where
    newDir = calcularDirEspejo w0 normal

objEspejoRandom :: [Shape] -> Direction -> Direction -> Point3D -> StdGen -> Float -> (Obj,StdGen)
objEspejoRandom figuras w0 norm p gen step  = (snd $ obtenerPrimeraColision $ map (oneCollision (Ray p newDir)) figuras,gen')
  where
    dirW0 = calcularDirEspejo w0 norm
    newDir = normal $ dirW0 + Direction x y z
    [x,y,z] = take 3 $ randomRs (-step,step) gen
    gen' = snd $ split gen

objCristal :: [Shape] -> Direction -> Direction -> Float -> Float -> Point3D -> (Obj, Float)
objCristal figuras w0 normal n1 n2 p = (snd $ obtenerPrimeraColision $ map (oneCollision (Ray pFix newDir)) figuras, n2')
  where
    pFix = {- movePoint (escalateDir' 1000 newDir) -} p
    newDir = calcularDirCristal w0 normal n1 n2'
    n2' = if n1 == n2 then 1 else n2
-- Para planos el final devolver 1

--------------------------
-- FUNCIONES DE COLORES --
--------------------------

{-# INLINE mediaLRGB #-}
mediaLRGB :: [[RGB]] -> [RGB]
mediaLRGB = map mediaRGB . transpose

{-# INLINE mediaRGB #-}
mediaRGB :: [RGB] -> RGB
mediaRGB xs = divRGB (sumRGB xs) $ fromIntegral (length xs)

{-# INLINE sumRGB #-}
sumRGB :: [RGB] -> RGB
sumRGB [] = RGB 0 0 0
sumRGB xs = foldr (+) (head xs) (tail xs)


{-# INLINE formula #-}
formula :: RGB -> Float -> Point3D -> Point3D -> Direction -> RGB -> Bool -> RGB
formula rgbLuz intLuz pointLuz p vNormal rgbObj tCos
  | (vNormal .* normal (pointLuz #< p)) < 0 = RGB 0 0 0
  | p == pointLuz = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/5.0))**2)) * rgbObj
  | tCos = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/5.0))**2)) * rgbObj `modRGB` (vNormal .* normal (pointLuz #< p)) -- "Integral"
  | otherwise = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/5.0))**2)) * rgbObj `modRGB` (pi * (vNormal .* normal (pointLuz #< p)))

--------------------------
-- FUNCIONES DE CAMARA  --
--------------------------

tuplasAleatorias :: [(Float, Float)] -> Float -> StdGen  -> [(Float, Float)]
tuplasAleatorias inputTuplas salto gen = [(x + r1, y + r2) | ((x, y), r1, r2) <- zip3 inputTuplas (take halfLen randomNumbers) (drop halfLen randomNumbers)]
  where
    !randomNumbers = take (length inputTuplas * 2) $ randomRs (0.0, salto) gen :: [Float]
    !halfLen = length randomNumbers `div` 2

{-# INLINE generarTuplas #-}
generarTuplas :: [Float] -> [Float] -> [(Float, Float)]
generarTuplas !xs !ys = [(x, y) | y <- ys, x <- xs]

--Genera los rayos para cada pixel
generateRaysForPixels :: Int -> Int -> Int -> Int -> Camara -> Float -> Float -> Int -> StdGen -> [Ray]
generateRaysForPixels maxN etapasX n etapaX (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height j gen =
  map (\(x, y) -> Ray p (generateDirection x y focal)) tuplasRandom
  where
    piY = py / height
    piX = px / width
    px' = px / 2.0
    py' = py / 2.0
    yValues = [py', (py'-piY) .. (-py'+piY)]
    yStep = length yValues `div` maxN
    startIdxy = n * yStep
    endIdxy = (n + 1) * yStep
    selectedYValues = take (endIdxy - startIdxy) (drop startIdxy yValues)
    generateDirection !width !height !focal = normal $ pointDir $ Point3D width height focal
    xValues = [(-px'), (-px'+piX) .. (px'-piX)]
    xStep = length xValues `div` etapasX
    startIdxx = etapaX * xStep
    endIdxx = (etapaX + 1) * xStep
    selectedxValues = take (endIdxx - startIdxx) (drop startIdxx xValues)
    !tuplas = generarTuplas  (concatMap (replicate j) selectedxValues) selectedYValues
    !tuplasRandom = tuplasAleatorias tuplas piY gen

--------------------------
--FUNCIONES DE PUNTOS.AL--
--------------------------

{-# INLINE polarToCartesian #-}
polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian !inclinacion !azimut !cosRand = Point3D (sin inclinacion * cos azimut) cosRand (sin inclinacion * sin azimut)

genPointTotal :: StdGen -> StdGen -> Point3D
genPointTotal gen1 gen2 = polarToCartesian (acos randIncl) (2.0 * pi * randAz) randIncl
  where
    !(randIncl, _) = randomR (-1.0, 1.0) gen1 :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen2 :: (Float, StdGen)

genPoint :: StdGen -> StdGen -> Point3D
genPoint gen1 gen2 = polarToCartesian (acos randIncl) (2 * pi * randAz) randIncl
  where
    !(randIncl, _) = randomR (0.0, 1.0) gen1 :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen2 :: (Float, StdGen)


gen2Point :: StdGen -> StdGen -> (Float,Float)
gen2Point gen1 gen2 = (u, v)
  where
    !(u, _) = randomR (0.0, 1.0) gen1 :: (Float, StdGen)
    !(v, _) = randomR (0.0, u) gen2 :: (Float, StdGen)
--------------------------
--FUNCIONES DE COLISION --
--------------------------

--Devuelve la primera colision de cada lista de colisiones
{-# INLINE obtenerPrimeraColision #-}
obtenerPrimeraColision :: [(Float, Obj)] -> (Float, Obj)
obtenerPrimeraColision !xs =
  case filter (\(x, _) -> x >= 0) xs of
    [] -> (-1,Obj (RGB 0 0 0) (Direction 0 0 0) (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)
    filteredList -> minimumBy (comparing fst) filteredList

-- dada la lista de listas de colisiones, devuelve la lista de la primera colisión de cada rayo
{-# INLINE listRay #-}
listRay :: [[(Float, Obj)]] -> [Obj]
listRay = map (snd . obtenerPrimeraColision) . transpose

{-# INLINE colision #-}
colision :: Point3D -> Point3D -> [Shape] -> Bool
colision !p0 !luz !figuras = aproxPoint p0 bonk -- Si es el mismo punto, no choca con nada
  where
    bonk = colObj $ snd $ obtenerPrimeraColision $ map (oneCollision (Ray luz (normal $ p0 #< luz))) figuras --Saca el punto de la primera colision de la luz con las figuras

--------------------------
--FUNCIONES DE DIRECCIONES
--------------------------

{-# INLINE calcularDirEspejo #-}
calcularDirEspejo :: Direction -> Direction -> Direction
calcularDirEspejo !d !normal = d - escalateDir (2.0 * (d .* normal)) normal

{-# INLINE calcularDirCristal #-}
calcularDirCristal :: Direction -> Direction -> Float -> Float -> Direction
calcularDirCristal !d !norm n1 n2 = if sinT2 > 1 then d else d'
 where
    n = n1 / n2
    cosI = -(d .* norm)
    !sinT2 = n * n * (1 - cosI * cosI)
    d' = normal $ escalateDir n d + escalateDir (n * cosI - sqrt (1 - sinT2)) norm

--------------------------
-- FUNCIONES AUXILIARES --
--------------------------

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

ruletaRusa :: (Float, Float, Float) -> StdGen-> (Int, Float)
ruletaRusa (a,b,c) gen = (i, p')
  where
    absorption = if a+b+c == 1 then 0.1 else 1-(a+b+c)
    d = a + b + c + absorption
    a' = a/d
    b' = (a + b)/d
    c' = (a + b + c)/d
    (p, _) = randomR (0.0, 1.0) gen :: (Float, StdGen)
    i | p < a' = 0
      | p < b' = 1
      | p < c' = 2
      | otherwise = 3
    p'
      | p < a' = a/d
      | p < b' = b/d
      | p < c' = c/d
      | otherwise = absorption

brdf :: Obj -> RGB
brdf (Obj {..}) =
  if idObj == 5
  then rgbTxt
  else scale rgbObj
  where
    -- Corrección temporal :D
    (x', y') = ((xP colObj + 25) / 50, (yP colObj + 25) / 50)
    texX = round $ x' * (texWidth - 1)
    texY = round $ y' * (texHeight - 1)
    textureImage = loadTexture "../meshes/algo.png"
    (texWidth, texHeight) = (fromIntegral $ imageWidth textureImage, fromIntegral $ imageHeight textureImage)
    rgbTxt = pixtoRGB $ pixelAt textureImage texX texY

sumFlLuz :: [Luz] -> Float
sumFlLuz [] = 0
sumFlLuz ((Luz _ _ int):luz) = int + sumFlLuz luz

loadTexture :: FilePath -> Image PixelRGB8
loadTexture filePath =
    unsafePerformIO $ do
        eitherImage <- readImage filePath
        case eitherImage of
            Right (ImageRGB8 img) -> return img
            Left err -> error $ "Error loading image: " ++ err
            _ -> error "Usa png ;)"

pixtoRGB :: PixelRGB8 -> RGB
pixtoRGB (PixelRGB8 r g b) = RGB (toFloat r) (toFloat g) (toFloat b)
  where
    toFloat :: Word8 -> Float
    toFloat x = fromIntegral x / 255.0

-- Interpolación lineal.
interpolate :: (Float, Float) -> Float -> Float
interpolate (a, b) t = a + t * (b - a)

-- Calcular la media de una lista de valores
{-# INLINE media #-}
media :: [Float] -> Float
media xs = sum xs / fromIntegral (length xs)

-- Calcular la varianza de una lista de valores
{-# INLINE varianza #-}
varianza :: [Float] -> Float
varianza xs = sum (map (\x -> (x - m) ^ 2) xs) / fromIntegral (length xs)
  where m = media xs

-- Calcular la desviación estándar de una lista de valores
{-# INLINE desviacionEstandar #-}
desviacionEstandar :: [Float] -> Float
desviacionEstandar xs = sqrt (varianza xs)

{-# INLINE fGaus #-}
fGaus :: [Foton] -> Obj -> Foton -> Float
fGaus photons obj fot = if isNaN result then 0 else result
  where
    !list = map (distFot (colObj obj)) photons
    a = 1 / (c * sqrt (2*pi))
    b = media list
    c = desviacionEstandar list
    x = distFot (colObj obj) fot
    result = a * exp (-(((x-b)**2) / (2*c**2)))

addNiebla :: Point3D -> Obj -> Float -> RGB -> RGB
addNiebla p obj x rgb = newRGB + (rgb `modRGB` reducObj) + (scale (RGB 30 30 40 )`modRGB` ((1-reducObj)*0.6))
  where
    newRGB = RGB fact fact fact
    reducLuz = if zP p < 0 then exp (x * zP p) else 1
    reducObj = if zP (colObj obj) < 0 then exp ((1-x) * zP (colObj obj)/7.5) else 1 -- Como le afecta la niebla de lejos a los objetos

    camP = Point3D 0 0 30 -- Comienzo de la camara
    cam = Ray camP dir
    dir = normal $ colObj obj #< camP
    fact = x * reducLuz * ((distanceToRay p cam / 25) ** (-1.75)) -- Como afecta la luz a los objetos

distanceToRay :: Point3D -> Ray -> Float
distanceToRay point ray =
  let
    !(Point3D ox oy oz) = oR ray
    !(Direction dx dy dz) = dR ray
    !px = ox - xP point
    !py = oy - yP point
    !pz = oz - zP point
    !a = dx * dx + dy * dy + dz * dz
    !b = px * dx + py * dy + pz * dz
    !t = -(b / a)
    !closestPoint = Point3D (ox + t * dx) (oy + t * dy) (oz + t * dz)
    !distance = distPoint point closestPoint
  in distance
