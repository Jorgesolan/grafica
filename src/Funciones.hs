{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Funciones where

import Codec.Picture
import Elem3D
    ( RGB(..),
      Luz(..),
      Base(..),
      Ray(..),
      Direction(..),
      Point3D(..),
      (#<),
      aproxPoint,
      (.*),
      escalateDir, escalateDir',
      modd,
      normal,
      modRGB,
      divRGB,
      scale,
      generateBase,
      cambioBase, addPoints, pointDir, Foton, distFot, (#), escalatePoint,
      distPoint, movePoint, angleBetween )
import Figuras
    ( oneCollision,
      Camara(..),
      Obj(..),
      Rectangulo(Rectangulo),
      Shape(Rectangle), getShapeID, getUV )

--import Math.Erf (erf)
import System.IO.Unsafe (unsafePerformIO)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Data.List (transpose)
import System.Random (randomR, StdGen, randomRs,split, mkStdGen)

import Data.Binary (Word8)
import Data.Number.Erf
import Data.Foldable (toList)
import qualified Data.DList as DL
import qualified Data.Set as Set

--------------------------
--  FUNCIONES LIBRERIA  --
--------------------------
{-# INLINE build #-}
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

{-# INLINE chunksOf #-}
--Divide una lista en n sublistas
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n


--------------------------
-- FUNCIONES DE OBJETOS --
--------------------------

{-# INLINE objAleatorio #-}
objAleatorio :: Set.Set Shape -> Obj -> StdGen -> Obj
objAleatorio figuras obj gen = nxtObj
  where
    !nxtObj = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray (colObj obj) $ normal (puntoAl #< colObj obj))) figuras
    !puntoAl = cambioBase (colObj obj) (generateBase dirAl (normObj obj) (normal (dirAl * normObj obj))) $ genPoint gen
    !dirAl = normal $ normObj obj * Direction 2 1 (-2)

{-# INLINE objEspejo #-}
objEspejo :: Set.Set Shape -> Direction -> Direction -> Point3D -> Obj
objEspejo figuras w0 norm p = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray p newDir)) figuras
  where
    newDir = dirEspejo w0 norm

objEspejoRandom :: Set.Set Shape -> Direction -> Direction -> Point3D -> StdGen -> Float -> (Obj,StdGen)
objEspejoRandom figuras w0 norm p gen step  = (obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray p newDir)) figuras,gen')
  where
    dirW0 = dirEspejo w0 norm
    newDir = normal $ dirW0 + Direction x y z
    [x,y,z] = take 3 $ randomRs (-step,step) gen
    gen' = snd $ split gen

{-# INLINE objCristal #-}
objCristal :: Set.Set Shape -> Direction -> Direction -> Float -> Float -> Point3D -> (Obj, Float)
objCristal figuras w0 norm n1 n2 p = {- trace (show newDir ++ " " ++ show nxtObj) $ -} (nxtObj, n2)
  where
    nxtObj = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray pFix newDir)) figuras
    pFix = movePoint newDir p
    newDir = normal $ calcularDirCristal w0 norm n1 n2

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
formula :: RGB -> Float -> Point3D -> Point3D -> Direction -> RGB -> RGB
formula rgbLuz intLuz pointLuz p vNormal rgbObj
  | (vNormal .* normal (pointLuz #< p)) < 0 = RGB 0 0 0
  | p == pointLuz = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/10.0))**2)) * rgbObj
  | otherwise = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/10.0))**2)) * rgbObj `modRGB` (vNormal .* normal (pointLuz #< p))

--------------------------
-- FUNCIONES DE CAMARA  --
--------------------------
{-# INLINE tuplasAleatorias #-}
tuplasAleatorias :: [(Float, Float)] -> Float -> StdGen -> [(Float, Float)]
tuplasAleatorias inputTuplas salto gen =
  [(x + r1, y + r2) | ((x, y), r1, r2) <- zip3 inputTuplas (take halfLen randomNumbers) (drop halfLen randomNumbers)]
  where
    randomNumbers = take (length inputTuplas * 2) $ randomRs (0.0, salto) gen :: [Float]
    halfLen = length randomNumbers `div` 2

{-# INLINE generarTuplas #-}
generarTuplas :: [Float] -> [Float] -> [(Float, Float)]
generarTuplas !xs !ys =[(x, y) | y <- ys, x <- xs]

{-# INLINE generateRaysForPixels #-}
generateRaysForPixels :: Int -> Int -> Int -> Int -> Camara -> Float -> Float -> Int -> StdGen -> [Ray]
generateRaysForPixels maxN etapasX n etapaX (Camara p (Base {..})) width height j gen =
  map (\(x, y) -> Ray p (generateDirection x y (zD d2))) tuplasRandom
  where
    px = xD d0
    py = yD d1
    piY = py / height
    piX = px / width
    px' = px / 2.0
    py' = py / 2.0
    yValues = [py', (py' - piY) .. (-py' + piY)]
    yStep = length yValues `div` maxN
    startIdxy = n * yStep
    endIdxy = (n + 1) * yStep
    selectedYValues = take (endIdxy - startIdxy) (drop startIdxy yValues)
    generateDirection !width !height !focal = normal $ pointDir $ Point3D width height (zD d2) # p
    xValues = [(-px'), (-px' + piX) .. (px' - piX)]
    xStep = length xValues `div` etapasX
    startIdxx = etapaX * xStep
    endIdxx = (etapaX + 1) * xStep
    selectedxValues = take (endIdxx - startIdxx) (drop startIdxx xValues)
    !tuplas = generarTuplas (concatMap (replicate j) selectedxValues) selectedYValues
    !tuplasRandom = tuplasAleatorias tuplas piY gen
--------------------------
--FUNCIONES DE PUNTOS.AL--
--------------------------

{-# INLINE polarToCartesian #-}
polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian !inclinacion !azimut !cosRand = Point3D (sin inclinacion * cos azimut) cosRand (sin inclinacion * sin azimut)

{-# INLINE genPointTotal #-}
genPointTotal :: StdGen -> Point3D
genPointTotal gen = polarToCartesian (acos randIncl) (2.0 * pi * randAz) randIncl
  where
    !(randIncl, gen') = randomR (-1.0, 1.0) gen :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen' :: (Float, StdGen)

{-# INLINE genPoint #-}    
genPoint :: StdGen -> Point3D
genPoint gen = polarToCartesian (acos randIncl) (2 * pi * randAz) (sqrt randIncl) -- (1 - randIncl)
  where
    !(randIncl, gen') = randomR (0.0, 1.0) gen :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen' :: (Float, StdGen)


--------------------------
--FUNCIONES DE COLISION --
--------------------------

--Devuelve la primera colision de cada lista de colisiones
{-# INLINE obtenerPrimeraColision #-}
obtenerPrimeraColision :: Set.Set Obj -> Obj
obtenerPrimeraColision xs =
  case Set.lookupMin $ Set.filter (\obj -> mindObj obj >= 0) xs of
    Nothing -> (Obj (-1) (RGB 0 0 0) (Direction 0 0 0) (Point3D 0 0 0) (Direction 0 0 0) (0, 0, 0) 0 0)
    Just obj -> obj

-- dada la lista de listas de colisiones, devuelve la lista de la primera colisión de cada rayo
{-# INLINE listRay #-}
listRay :: Set.Set Obj -> Obj
listRay = obtenerPrimeraColision

{-# INLINE colision #-}
colision :: Point3D -> Point3D -> Set.Set Shape -> Bool
colision !p0 !luz !figuras = aproxPoint p0 bonk -- Si es el mismo punto, no choca con nada
  where
    bonk = colObj $ obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray luz (normal $ p0 #< luz))) figuras --Saca el punto de la primera colision de la luz con las figuras

--------------------------
--FUNCIONES DE DIRECCIONES
--------------------------

{-# INLINE dirEspejo #-}
dirEspejo :: Direction -> Direction -> Direction
dirEspejo !d !norm = normal $  d - escalateDir (2.0 * (d .* norm)) norm

{-# INLINE calcularDirCristal #-}
calcularDirCristal :: Direction -> Direction -> Float -> Float -> Direction
calcularDirCristal !d !norm n1 n2 = if sinT2 > 1 then d else d'
 where
    n = n1 / n2
    cosI = -(d .* norm)
    !sinT2 = (n * n) * (1 - (cosI * cosI))
    d' = normal $ escalateDir n d + escalateDir (n * cosI - sqrt (1 - sinT2)) norm

--------------------------
-- FUNCIONES AUXILIARES --
--------------------------
{-# INLINE ruletaRusa #-}
ruletaRusa :: (Float, Float, Float) -> StdGen -> (Int, Float)
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

{-# INLINE brdf #-}
brdf :: Obj -> Set.Set Shape-> RGB
brdf (Obj {..}) figuras
  | idObj == 4 = scale rgbObj -- rgbTxt
  | kd == 0 = RGB 1 1 1 -- Que a los cristales o espejos sin difusa no le afecte un color que le hayas puesto
  | otherwise = scale rgbObj
  where
      (kd, ke, kr) = trObj
      (x', y') = ((xP colObj + 10) / 50, (yP colObj + 10) / 50)
      textureImage = loadTexture "../meshes/algo.png"
      (texWidth, texHeight)
        = (fromIntegral $ imageWidth textureImage,
           fromIntegral $ imageHeight textureImage)
      rgbTxt = pixtoRGB $ pixelAt textureImage (round $ u * (texWidth - 1)) (round $  v * (texHeight - 1))
      fig = head $ filter (\shape -> idObj == getShapeID shape) (Set.toList figuras)
      (u,v) = getUV fig colObj


loadTexture :: FilePath -> Image PixelRGB8
loadTexture filePath =
    unsafePerformIO $ do
        eitherImage <- readImage filePath
        case eitherImage of
            Right (ImageRGB8 img) -> return img
            Left err -> error $ "Error loading image: " ++ err
            _ -> error "Usa png ;)"

{-# INLINE pixtoRGB #-}
pixtoRGB :: PixelRGB8 -> RGB
pixtoRGB (PixelRGB8 r g b) = RGB (toFloat r) (toFloat g) (toFloat b)
  where
    toFloat :: Word8 -> Float
    toFloat x = fromIntegral x / 255.0

{-# INLINE sumFlLuz #-}
sumFlLuz :: [Luz] -> Float
sumFlLuz [] = 0
sumFlLuz ((Luz _ _ int):luz) = int + sumFlLuz luz

--------------------------
--FUNCIONES ESTADISTICAS--
--------------------------

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


--------------------------
--       EXTRAS         --
--------------------------
{-# INLINE addNiebla #-}
addNiebla :: Luz -> Obj -> Float -> Set.Set Shape ->  RGB -> RGB
addNiebla (Luz p _ _) obj x figuras rgb = if (mindObj obj ) < 0 then RGB 0 0 0 else newRGB + (rgb `modRGB` reducObj)
  where
    newRGB = RGB fact fact fact
    reducLuz = if zP closest < 0 then exp (x * zP closest / 10) else 1
    reducObj = if zP (colObj obj) < 0 then exp ((1-x) * zP (colObj obj) / 10) else 1 -- Como le afecta la niebla de lejos a los objetos

    camP = Point3D 0 0 10 -- Comienzo de la camara
    cam = Ray camP dir
    dir = normal $ colObj obj #< camP
    closest = distanceToRay p cam
    fact = (1-x) * reducLuz * (distPoint p closest ** (-1.75)) -- Como afecta la luz a los objetos

{-# INLINE distanceToRay #-}
distanceToRay :: Point3D -> Ray -> Point3D -- Punto más cercano al rayo
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
    in closestPoint

{-# INLINE fPhong #-}
fPhong :: Point3D -> Obj -> Float -> Set.Set Shape -> Float
fPhong pLuz obj alpha figuras  = if col then (alpha+2/2) *abs (dirEspejo (colObj obj #<pLuz) (normObj obj) .* w0Obj obj)**alpha else 0
  where
    col = colision (colObj obj) pLuz figuras'
    figuras' = Set.filter (\shape -> idObj obj /= getShapeID shape) figuras

-- iris :: Obj -> RGB
-- iris (Obj {..}) = newRGB
--   where
--     newRGB = RGB (1 * rFactor') (1 * gFactor') (1 * bFactor')
--     angle = abs $ normObj .* w0Obj
--     thickness = 0.3
--     rFactor' = rFactor
--     bFactor' = bFactor
--     gFactor' = gFactor
--     -- Factores de ajuste para cada canal
--     rFactor = abs $ interferenceFactor * cos (2 * pi * thickness * refractiveIndex)
--     gFactor = abs $ interferenceFactor * cos (2 * pi * thickness * refractiveIndex)
--     bFactor = abs $ interferenceFactor * cos (2 * pi * thickness * refractiveIndex)

--     -- Ajusta estos parámetros según tu necesidad
--     interferenceFactor = 0.2
--     refractiveIndex = reflObj

-- fresnell :: Obj -> Float -> Float
-- fresnell (Obj {..}) iR = 0.5 * (paralelo**2 + perpendicular**2)
--   where
--     paralelo = (reflObj* cosI - iR * cosT) / (reflObj* cosI + iR * cosT)
--     perpendicular = (iR* cosI - reflObj * cosT) / (iR* cosI + reflObj * cosT)
--     cosI = w0Obj .* normObj
--     cosT = normObj .* calcularDirCristal w0Obj normObj iR reflObj

-- microfacet :: Direction -> Direction -> Float -> Float
-- microfacet wH norm alpha = exp (- (sqrt (1 - cos**2) / cos) / alpha**2) / (pi * alpha**2 * cos**4 )
--   where cos = min (wH .* norm) 1

-- shadowing :: Direction -> Direction -> Float -> Float
-- shadowing wI norm alpha = 2 / (1 + erf s + 1/(s*sqrt pi) + exp (-(s**2)))
--   where
--     s = abs $ 1 / (alpha * tan)
--     tan = sqrt (1 - cos**2) / cos
--     cos = min (wI .* norm) 1

{-# INLINE mulCam #-}
mulCam :: Camara -> Int -> Float -> [Camara]
mulCam cam@(Camara p b) n radio = cam : map (`Camara` b) (take n transformedPoints)
  where
    circlePoints = pointsInUnitCircle  -- Tomamos todos los puntos dentro de un círculo unitario
    
    -- Función para escalar y desplazar puntos según el círculo deseado
    transformPoint (x, y) = Point3D (radio * x) (radio * y) 20

    -- Lista de puntos dentro de un círculo unitario
    pointsInUnitCircle = [(cos theta, sin theta) | theta <- [0, (2 * pi) / fromIntegral n .. 2 * pi]]

    -- Aplicar la transformación a cada punto
    transformedPoints = map transformPoint circlePoints