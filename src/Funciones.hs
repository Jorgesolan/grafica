{-# LANGUAGE BangPatterns #-}
module Funciones where

import Elem3D
import Figuras

import Data.Ord (comparing)
import Debug.Trace (trace)
import Data.List (minimumBy,transpose)
import System.Random (randomR, StdGen, randomRs)

objAleatorio :: [Shape] -> Obj -> StdGen -> StdGen -> Obj
objAleatorio figuras obj@(Obj _ !p norm _ _) gen gen' = nxtObj
  where
    !nxtObj = snd $ obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray p (normal(puntoAl #< p)))) figuras -- Siguiente objeto que choca, q no sea el mismo
    puntoAl = cambioBase p (generateBase dirAl norm (normal (dirAl * norm))) (genPoint gen gen')
    dirAl = normal $ norm * Direction 2 1 (-2) -- Direccion cualquiera para que no se repita, si peta cambiar esto

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
polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian !inclinacion !azimut !cosRand = Point3D (sin inclinacion * cos azimut) cosRand (sin inclinacion * sin azimut)

genPointTotal :: StdGen -> StdGen -> Point3D
genPointTotal gen1 gen2 = polarToCartesian (acos cosRand) (2 * pi * randAz) cosRand
  where
    !(randIncl, _) = randomR (0.0, 1.0) gen1 :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen2 :: (Float, StdGen)
    !cosRand = sqrt $ 1 - randIncl

genPoint :: StdGen -> StdGen -> Point3D
genPoint gen1 gen2 = polarToCartesian (acos cosRand) (2 * pi * randAz) cosRand
  where
    !(randIncl, _) = randomR (0.0, 1.0) gen1 :: (Float, StdGen)
    !(randAz, _) = randomR (0.0, 1.0) gen2 :: (Float, StdGen)
    !cosRand = sqrt $ 1 - randIncl

tuplasAleatorias :: [(Float, Float)] -> Float -> StdGen  -> [(Float, Float)]
tuplasAleatorias inputTuplas salto gen = [(x + r1, y + r2) | ((x, y), r1, r2) <- zip3 inputTuplas (take halfLen randomNumbers) (drop halfLen randomNumbers)]
  where
    !randomNumbers = take (length inputTuplas * 2) $ randomRs (0.0, salto) gen :: [Float]
    !halfLen = length randomNumbers `div` 2

{-# INLINE generarTuplas #-}
generarTuplas :: [Float] -> [Float] -> [(Float, Float)]
generarTuplas !xs !ys = [(x, y) | y <- ys, x <- xs]

--Genera los rayos para cada pixel
generateRaysForPixels :: Int -> Int -> Camara -> Float -> Float -> Float -> StdGen -> [Ray]
generateRaysForPixels maxN n (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height j gen =
  map (\(x, y) -> Ray p (generateDirection x y focal)) tuplasRandom
  where
    !piY = py / height
    !piX = px / width
    !px' = px / 2
    !py' = py / 2
    !yValues = [(py'), (py'-piY) .. (-py'+piY)]
    !yStep = (length yValues) `div` maxN
    startIdx = (n - 1) * yStep
    endIdx = n * yStep
    selectedYValues = take (endIdx - startIdx) (drop startIdx yValues)
    generateDirection !width !height !focal = normal $ (Point3D width height focal) #< p
    !tuplas = generarTuplas  (concatMap (replicate (round j))[(-px'), (piX-px') ..(px'-piX)]) selectedYValues
    !tuplasRandom = tuplasAleatorias tuplas piY gen

--Devuelve la primera colision de cada lista de colisiones
{-# INLINE obtenerPrimeraColision #-}
obtenerPrimeraColision :: [(Float, Obj)] -> (Float, Obj)
obtenerPrimeraColision !xs = 
  case filter (\(x, _) -> x >= 0) xs of
    [] -> (-1,(Obj (RGB 0 0 0) (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0))
    filteredList -> (minimumBy (comparing fst) filteredList)

-- dada la lista de listas de colisiones, devuelve la lista de la primera colisión de cada rayo
{-# INLINE listRay #-}
listRay :: [[(Float, Obj)]] -> [Obj]
listRay = map (snd . obtenerPrimeraColision) . transpose

{-# INLINE calcularDirEspejo #-}
calcularDirEspejo :: Direction -> Direction -> Direction
calcularDirEspejo !d !normal = d - (escalateDir (2 * (d .* normal)) normal)

calcularDirCristal :: Direction -> Direction -> Float -> Float -> Direction
calcularDirCristal !d !norm n1 n2 = if sinT2 > 1 then d else d''
 where
    n = n1 / n2
    cosI = -(d .* norm)
    !sinT2 = n * n * (1 - cosI * cosI)
    d'' = normal $ escalateDir n d + escalateDir (n * cosI - sqrt (1 - sinT2)) norm


{-# INLINE formula #-}
formula :: RGB -> Float -> Point3D -> Point3D -> Direction -> RGB -> Bool -> RGB
formula rgbLuz intLuz pointLuz p vNormal brdf tCos 
  {- | (vNormal .* (normal (p #< pointLuz))) < 0 = 0 -}
  | tCos = (modRGB rgbLuz $ intLuz / ((1+(modd (p #< pointLuz)/25))**2)) * brdf `modRGB` abs (vNormal .* (normal (p #< pointLuz))) -- "Integral"
  | otherwise = (modRGB rgbLuz $ intLuz / ((1+(modd (p #< pointLuz)/25))**2)) * brdf `modRGB` pi

{-# INLINE colision #-}
colision :: Point3D -> Point3D -> [Shape] -> Bool
colision !p0 !luz !figuras = aproxPoint p0 bonk -- Si es el mismo punto, no choca con nada
  where
    bonk = obtenerPunto $ snd $ obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray luz (normal $ p0 #< luz))) figuras --Saca el punto de la primera colision de la luz con las figuras


ruletaRusa :: (Float, Float, Float) -> StdGen-> (Int, Float)
ruletaRusa (a,b,c) gen = (i, p')
  where
    d = a + b + c + 0.15
    a' = a/d
    b' = (a + b)/d
    c' = (a + b + c)/d
    (p, _) = randomR (0.0, 1.0) gen :: (Float, StdGen)
    i = if p < a' then 0 else (if p < b' then 1 else (if p < c' then 2 else 3))
    p' = if p < a' then a/d else (if p < b' then b/d else (if p < c' then c/d else 0.15/d))

brdf :: Obj -> RGB
brdf (Obj rgb _ _ (kd,kr,ke) _ ) 
  | kd == 1 = rgb
  | ke == 1 = RGB 255 255 255
  | otherwise = rgb
