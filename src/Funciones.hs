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
      Shape(Null,Triangle),
       getShapeID, getUV )
import Tone_map(gammaFunc')

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
--  FunciónES LIBRERIA  --
--------------------------
-- | Función auxiliar, divide una lista en n sublistas.
{-# INLINE chunksOf #-}
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

    build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
    build g = g (:) []


--------------------------
-- FunciónES DE OBJETOS --
--------------------------

-- | Función auxiliar, que devuelve un objeto aleatorio tirado desde otro objeto.
{-# INLINE objAleatorio #-}
objAleatorio :: Set.Set Shape -> Obj -> StdGen -> Obj
objAleatorio figuras obj gen = nxtObj
  where
    !nxtObj = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray (colObj obj) $ normal (puntoAl #< colObj obj))) figuras
    !puntoAl = cambioBase (colObj obj) (generateBase dirAl (normObj obj) (normal (dirAl * normObj obj))) $ genPoint gen
    !dirAl = normal $ normObj obj * Direction 2 1 (-2)

-- | Función auxiliar, que devuelve el siguiente objeto después de chocar con un objeto con propiedades de espejo.
{-# INLINE objEspejo #-}
objEspejo :: Set.Set Shape -> Direction -> Direction -> Point3D -> Obj
objEspejo figuras w0 norm p = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray p newDir)) figuras
  where
    newDir = dirEspejo w0 norm

-- | Función auxiliar, que devuelve el siguiente objeto después de chocar con un objeto con propiedades de cristal.
{-# INLINE objCristal #-}
objCristal :: Set.Set Shape -> Direction -> Direction -> Float -> Float -> Point3D -> (Obj, Float)
objCristal figuras w0 norm n1 n2 p = {- trace (show newDir ++ " " ++ show nxtObj) $ -} (nxtObj, n2)
  where
    nxtObj = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray pFix newDir)) figuras
    pFix = movePoint newDir p
    newDir = normal $ calcularDirCristal w0 norm n1 n2

--------------------------
-- FunciónES DE COLORES --
--------------------------

-- | Función auxiliar, que realiza la media de una una matriz a una lista.
{-# INLINE mediaLRGB #-}
mediaLRGB :: [[RGB]] -> [RGB]
mediaLRGB = map mediaRGB . transpose

{-# INLINE mediaRGB #-}
-- | Función auxiliar, que realiza la media RGB de una lista de RGBs dados.
mediaRGB :: [RGB] -> RGB
mediaRGB xs = divRGB (sumRGB xs) $ fromIntegral (length xs)

{-# INLINE sumRGB #-}
-- | Función auxiliar, que realiza el sumatorio RGB de una lista de RGBs dados.
sumRGB :: [RGB] -> RGB
sumRGB [] = RGB 0 0 0
sumRGB xs = foldr (+) (head xs) (tail xs)


{-# INLINE formula #-}
-- | Función básica, calcula la formula de render.
formula :: RGB -> Float -> Point3D -> Point3D -> Direction -> RGB -> RGB
formula rgbLuz intLuz pointLuz p vNormal rgbObj
  | (vNormal .* normal (pointLuz #< p)) < 0 = RGB 0 0 0
  | p == pointLuz = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/30))**2)) * rgbObj
  | otherwise = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/30))**2)) * rgbObj `modRGB` (vNormal .* normal (pointLuz #< p))

--------------------------
-- FunciónES DE CAMARA  --
--------------------------
{-# INLINE tuplasAleatorias #-}
-- | Función auxiliar, aplica un valor aleatorio € [0,salto], sobre la lista de tuplas float.
tuplasAleatorias :: [(Float, Float)] -> Float -> StdGen -> [(Float, Float)]
tuplasAleatorias inputTuplas salto gen =
  [(x + r1, y + r2) | ((x, y), r1, r2) <- zip3 inputTuplas (take halfLen randomNumbers) (drop halfLen randomNumbers)]
  where
    randomNumbers = take (length inputTuplas * 2) $ randomRs (0.0, salto) gen :: [Float]
    halfLen = length randomNumbers `div` 2

{-# INLINE generarTuplas #-}
-- | Función auxiliar, dadas dos listas genera todas las permutaciones de estas generando todas las combinaciones de 2 a 2. 
generarTuplas :: [Float] -> [Float] -> [(Float, Float)]
generarTuplas !xs !ys =[(x, y) | y <- ys, x <- xs]

{-# INLINE generateRaysForPixels #-}
-- | Función básica, daados los datos de entrada, la camara y el tamaño de la imagen, lanza los rayos pertinentes desde la cámara.
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
    generateDirection !width !height !focal = normal $ pointDir $ Point3D (width - xP p ) (height - yP p) (zD d2 - zP p) 
    -- xValues = [(-px'), (-px' + piX) .. (px' - piX)]
    xValues = [(px'), (px' - piX) .. (-px' + piX)]

    xStep = length xValues `div` etapasX
    startIdxx = etapaX * xStep
    endIdxx = (etapaX + 1) * xStep
    selectedxValues = take (endIdxx - startIdxx) (drop startIdxx xValues)
    !tuplas = generarTuplas (concatMap (replicate j) selectedxValues) selectedYValues
    !tuplasRandom = tuplasAleatorias tuplas piY gen
--------------------------
--FunciónES DE PUNTOS.AL--
--------------------------

{-# INLINE polarToCartesian #-}
-- | Función auxiliar, convierte de coordenadas polares a coordenadas cartesianas.
polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian !inclinacion !azimut !cosRand = Point3D (sin inclinacion * cos azimut) cosRand (sin inclinacion * sin azimut)

{-# INLINE genPointTotal #-}
-- | Función auxiliar, genera un punto aleatorio sobre la superficie de una esfera.
genPointTotal :: StdGen -> Point3D
genPointTotal gen = polarToCartesian (acos randIncl) (2.0 * pi * randAz) randIncl
  where
    !(randIncl, gen') = randomR (-1.0, 1.0) gen :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen' :: (Float, StdGen)

{-# INLINE genPoint #-}
-- | Función auxiliar, genera un punto aleatorio sobre la superficie de una semiesfera.    
genPoint :: StdGen -> Point3D
genPoint gen = polarToCartesian (acos randIncl') (2 * pi * randAz) randIncl' 
  where
    !(randIncl, gen') = randomR (0.0, 1.0) gen :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen' :: (Float, StdGen)
    --randIncl' = sqrt(1 - randIncl) -- Luz Area
    randIncl' = randIncl -- Luz Puntual


--------------------------
--FunciónES DE COLISION --
--------------------------

--Devuelve la primera colision de cada lista de colisiones
{-# INLINE obtenerPrimeraColision #-}
-- | Función básica, dada una lista de colisiones devuelve la primera(por cercanía).
obtenerPrimeraColision :: Set.Set Obj -> Obj
obtenerPrimeraColision xs =
  case Set.lookupMin $ Set.filter (\obj -> mindObj obj >= 0) xs of
    Nothing -> (Obj (-1) (RGB 0 0 0) (Direction 0 0 0) (Point3D 0 0 0) (Direction 0 0 0) (0, 0, 0) 0 0 Null)
    Just obj -> obj

-- dada la lista de listas de colisiones, devuelve la lista de la primera colisión de cada rayo
{-# INLINE listRay #-}
-- | Función auxiliar, dada una lista de colisiones devuelve la primera(por cercanía)(probablemente sobra esta función :c).
listRay :: Set.Set Obj -> Obj
listRay = obtenerPrimeraColision

{-# INLINE colision #-}
-- | Función básica, dados dos puntos y una figura, devuelve si existe o no una colisión directa entre ellos.
colision :: Point3D -> Point3D -> Set.Set Shape -> Bool
colision !p0 !luz !figuras = aproxPoint p0 bonk -- Si es el mismo punto, no choca con nada
  where
    bonk = colObj $ obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray luz (normal $ p0 #< luz))) figuras --Saca el punto de la primera colision de la luz con las figuras

--------------------------
--FunciónES DE DIRECCIONES
--------------------------

{-# INLINE dirEspejo #-}
-- | Función básica, dada una dirección y la direción normal de un objeto, devuelve la dirección espejo.
dirEspejo :: Direction -> Direction -> Direction
dirEspejo !d !norm = normal $  d - escalateDir (2.0 * (d .* norm)) norm

{-# INLINE calcularDirCristal #-}
-- | Función básica,  dada una dirección y la direción normal de un objeto y los coeficientes de refracción, devuelve la dirección refractada.
calcularDirCristal :: Direction -> Direction -> Float -> Float -> Direction
calcularDirCristal !d !norm n1 n2 = if sinT2 > 1 then d else d'
 where
    n = n1 / n2
    cosI = -(d .* norm)
    !sinT2 = (n * n) * (1 - (cosI * cosI))
    d' = normal $ escalateDir n d + escalateDir (n * cosI - sqrt (1 - sinT2)) norm

--------------------------
-- FunciónES AUXILIARES --
--------------------------
-- | Función básica, depende de las caracterisicas del material, devuelve como se comporta el objeto y su probabilidad.
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

-- 
{-# INLINE brdf #-}
-- | Función básica, dicta de que color se comporta un objeto.
brdf :: Obj -> Set.Set Shape-> RGB
brdf obj@(Obj {..}) figuras
  -- | idObj == 7 =  rgbTxt "../meshes/gold.png"
  | idObj == 2 = rgbTxt "../meshes/algo.png"
  -- | idObj == 6 = rgbTxt "../meshes/wood.png"
  | kd == 0 = RGB 1 1 1 -- Que a los cristales o espejos sin difusa no le afecte un color que le hayas puesto
  | otherwise = scale rgbObj
  where
      (kd, ke, kr) = trObj
      rgbTxt path = getRGBTexture path obj

-- | Función auxiliar, devuelve el color de un objeto en una textura.
{-# INLINE getRGBTexture #-}
getRGBTexture :: String -> Obj -> RGB
getRGBTexture path (Obj {..}) = newRGB
  where
      textureImage = loadTexture path
      (texWidth, texHeight)
        = (fromIntegral $ imageWidth textureImage,
          fromIntegral $ imageHeight textureImage)
      newRGB = pixtoRGB $ pixelAt textureImage (round $ u * (texWidth - 1)) (round $  v * (texHeight - 1))
      (u,v) = getUV shObj colObj

-- | Función auxiliar, carga una textura  de un fichero.
loadTexture :: FilePath -> Image PixelRGB8
loadTexture filePath =
    unsafePerformIO $ do
        eitherImage <- readImage filePath
        case eitherImage of
            Right (ImageRGB8 img) -> return img
            Left err -> error $ "Error loading image: " ++ err
            _ -> error "Usa png ;)"

-- | Función auxiliar, convierte el tipo PixelRGB8 a RGB.
{-# INLINE pixtoRGB #-}
pixtoRGB :: PixelRGB8 -> RGB
pixtoRGB (PixelRGB8 r g b) = RGB (toFloat r) (toFloat g) (toFloat b)
  where
    toFloat :: Word8 -> Float
    toFloat x = fromIntegral x / 255.0

-- | Función auxiliar, suma los valores de las luces.
{-# INLINE sumFlLuz #-}
sumFlLuz :: [Luz] -> Float
sumFlLuz [] = 0
sumFlLuz ((Luz _ _ int):luz) = int + sumFlLuz luz

--------------------------
--FunciónES ESTADISTICAS--
--------------------------


-- | Calcular la media de una lista de valores.
{-# INLINE media #-}
media :: [Float] -> Float
media xs = sum xs / fromIntegral (length xs)

-- | Calcular la varianza de una lista de valores.
{-# INLINE varianza #-}
varianza :: [Float] -> Float
varianza xs = sum (map (\x -> (x - m) ^ 2) xs) / fromIntegral (length xs)
  where m = media xs

-- | Calcular la desviación estándar de una lista de valores.
{-# INLINE desviacionEstandar #-}
desviacionEstandar :: [Float] -> Float
desviacionEstandar xs = sqrt (varianza xs)

-- | Calcula mediante una Función Gaussiana el peso de un fotón.
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
-- | Función auxiliar, devuelve el color después de que un rayo pase a traves de niebla homogenea.
{-# INLINE addNiebla #-}
addNiebla :: Luz -> Obj -> Float -> Set.Set Shape ->  RGB -> RGB
addNiebla (Luz {..}) obj x figuras rgb = if (mindObj obj) < 0 {- || choca closest figuras luzP -} then RGB 0 0 0 else newRGB + (rgb `modRGB` reducObj)
  where
    rgb' = head $ gammaFunc' 1 2.4 [rgb]
    newRGB = RGB fact fact fact * (scale luzRGB)
    reducLuz = if zP closest < 0 then exp (x * zP closest / 30) else 1
    reducObj = if zP (colObj obj) < 0 then exp ((1-x) * zP (colObj obj) / 30) else 1 -- Como le afecta la niebla de lejos a los objetos

    camP = Point3D 0 0 35 -- Comienzo de la camara
    cam = Ray camP dir
    dir = normal $ (colObj obj) #< camP
    closest = distanceToRay luzP cam
    fact = (1-x) * reducLuz * (distPoint luzP closest ** (-1.75)) -- Como afecta la luz a los objetos

    choca :: Point3D -> Set.Set Shape -> Point3D -> Bool
    choca p figuras pLuz = ((mindObj bonk) < distLuz) && (distPoint pLuz (colObj bonk) < distPoint pLuz p)
      where
        bonk = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura (Ray pLuz (normal $ pLuz #< p))) figuras --Saca el punto de la primera colision de la luz con las figuras
        distLuz = distPoint pLuz p

-- | Función auxiliar, devuelve el punto en un rayo más cerca de un punto dado.
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

-- | Función auxiliar, brdf de Phong.
{-# INLINE fPhong #-}
fPhong :: Point3D -> Obj -> Float -> Set.Set Shape -> Float
fPhong pLuz obj alpha figuras  = if col then (alpha+2/2) * abs (dirEspejo (colObj obj #<pLuz) (normObj obj) .* w0Obj obj)**alpha else 0
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

-- | Función auxiliar, devuelve el porcentaje de especular de como se comporta un objeto después de aplicarle las ecuaciones de fresnell.
fresnell :: Obj -> Float -> Float
fresnell (Obj {..}) iR = 0.5 * (paralelo**2 + perpendicular**2)
  where
    paralelo = (reflObj* cosI - iR * cosT) / (reflObj* cosI + iR * cosT)
    perpendicular = (iR* cosI - reflObj * cosT) / (iR* cosI + reflObj * cosT)
    cosI = w0Obj .* normObj
    cosT = normObj .* calcularDirCristal w0Obj normObj reflObj iR

-- microfacet :: Direction -> Direction -> Float -> Float
-- microfacet wH norm alpha = exp (- (sqrt (1 - cos**2) / cos) / alpha**2) / (pi * alpha**2 * cos**4 )
--   where cos = min (wH .* norm) 1

-- shadowing :: Direction -> Direction -> Float -> Float
-- shadowing wI norm alpha = 2 / (1 + erf s + 1/(s*sqrt pi) + exp (-(s**2)))
--   where
--     s = abs $ 1 / (alpha * tan)
--     tan = sqrt (1 - cos**2) / cos
--     cos = min (wI .* norm) 1

-- | Función auxiliar, genera múltiples cámaras en un radio dado.
{-# INLINE mulCam #-}
mulCam :: Camara -> Int -> Float -> [Camara]
mulCam cam@(Camara p b) n radio = cam : map (`Camara` b) (take n transformedPoints)
  where
    circlePoints = pointsInUnitCircle  -- Tomamos todos los puntos dentro de un círculo unitario
    
    -- Función para escalar y desplazar puntos según el círculo deseado
    transformPoint (x, y) = Point3D (radio * x) (radio * y) (35)

    -- Lista de puntos dentro de un círculo unitario
    pointsInUnitCircle = [(cos theta, sin theta) | theta <- [0, (2 * pi) / fromIntegral n .. 2 * pi]]

    -- Aplicar la transformación a cada punto
    transformedPoints = map transformPoint circlePoints