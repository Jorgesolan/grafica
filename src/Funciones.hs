{-# LANGUAGE BangPatterns #-}
module Funciones where

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
      cambioBase )
import Figuras
    ( Obj(..), Shape, Camara(..), obtenerPunto, oneCollision )

import Data.Ord (comparing)
import Debug.Trace (trace)
import Data.List (minimumBy,transpose)
import System.Random (randomR, StdGen, randomRs)

--------------------------
-- FUNCIONES DE OBJETOS --
--------------------------

{-# INLINE objAleatorio #-}
objAleatorio :: [Shape] -> Obj -> StdGen -> StdGen -> Obj
objAleatorio figuras obj@(Obj _ _ !p norm _ _ _) gen gen' = nxtObj
  where
    !nxtObj = snd $ obtenerPrimeraColision $ map (oneCollision (Ray p $ normal (puntoAl #< p))) figuras -- Siguiente objeto que choca, q no sea el mismo
    puntoAl = cambioBase p (generateBase dirAl norm (normal (dirAl * norm))) $ genPoint gen gen'
    dirAl = normal $ norm * Direction 2 1 (-2) -- Direccion cualquiera para que no se repita, si peta cambiar esto

{-# INLINE objEspejo #-}
objEspejo :: [Shape] -> Direction -> Direction -> Point3D -> Obj
objEspejo figuras w0 normal p = snd $ obtenerPrimeraColision $ map (oneCollision (Ray p newDir)) figuras
  where
    newDir = calcularDirEspejo w0 normal

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
  | otherwise = modRGB rgbLuz (intLuz / ((1+(modd (pointLuz #< p)/5.0))**2)) * rgbObj `modRGB` pi

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
generateRaysForPixels :: Int -> Int -> Camara -> Float -> Float -> Int -> StdGen -> [Ray]
generateRaysForPixels maxN n (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height j gen =
  map (\(x, y) -> Ray p (generateDirection x y focal)) tuplasRandom
  where
    !piY = py / height
    !piX = px / width
    !px' = px / 2.0
    !py' = py / 2.0
    !yValues = [py', (py'-piY) .. (-py'+piY)]
    !yStep = length yValues `div` maxN
    startIdx = (n - 1) * yStep
    endIdx = n * yStep
    selectedYValues = take (endIdx - startIdx) (drop startIdx yValues)
    generateDirection !width !height !focal = normal $ Point3D width height focal #< p
    !tuplas = generarTuplas  (concatMap (replicate j) [(-px'), (piX-px') ..(px'-piX)]) selectedYValues
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
    bonk = obtenerPunto $ snd $ obtenerPrimeraColision $ map (oneCollision (Ray luz (normal $ p0 #< luz))) figuras --Saca el punto de la primera colision de la luz con las figuras

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
brdf (Obj rgb w0 p norm tr kr id ) = scale rgb

sumFlLuz :: [Luz] -> Float
sumFlLuz [] = 0
sumFlLuz ((Luz _ _ int):luz) = int + sumFlLuz luz
