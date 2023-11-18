{-# LANGUAGE BangPatterns #-}
module PathTracer where

import Elem3D 
import Figuras
import System.Random (StdGen, split)
import Funciones 
import Debug.Trace (trace)

pathTracer :: [Luz] -> [Shape] -> Float -> Float -> Obj -> StdGen -> RGB
pathTracer luz !figuras !n nMx obj@(Obj _ _ _ _ _ id) gen
  | nMx == 0 = colorDirecto * RGB 255 255 255 -- Solo luz directa
  | otherwise = (colorDirecto + colorIndirecto) * RGB 255 255 255 -- Luz directa + indirecta
   where
    colorDirecto = luzDirecta obj luz figuras True
    colorIndirecto = luzIndirecta obj luz figuras gen (n+1) nMx


luzDirecta :: Obj -> [Luz] -> [Shape] -> Bool -> RGB
luzDirecta obj luces figuras peso = case luces of
  []        -> RGB 0 0 0
  [x]       -> luzMono obj x figuras peso
  (x:xs)    -> (luzMono obj x figuras peso + luzDirecta obj xs figuras peso) `divRGB` 2


luzMono :: Obj -> Luz -> [Shape] -> Bool -> RGB
luzMono obj@(Obj rgb w0 p norm (kd,kr,ke) id) (Luz pointLuz rgbLuz intLuz) figuras peso 
  | ke > 0 = luzMono objEsp (Luz pointLuz rgbLuz intLuz) figuras True
  -- | kr > 0 = luzMono objCr (Luz pointLuz rgbLuz intLuz) figuras True
  | otherwise = colorDirecto
  where
    colorDirecto = if colision p pointLuz figuras then newRGB else RGB 0 0 0
    newRGB = formula (scale rgbLuz) intLuz pointLuz p norm (brdf obj) True -- "Integral"
    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    objEsp = snd $ obtenerPrimeraColision $ map (oneCollision (Ray p (calcularDirEspejo w0 norm))) figuras'

luzIndirecta :: Obj -> [Luz] -> [Shape] -> StdGen -> Float -> Float -> RGB
luzIndirecta obj@(Obj _ w0 p norm (kd,kr,ke) id) luz figuras gen n nMx 
  -- | kr > 0 = luzIndirecta objCr luz figuras gen' n nMx -- Rayo reflejado
  | ke > 0 = luzIndirecta objEsp luz figuras gen' n nMx
  | nMx == n = rgbNew colorDirecto $ brdf obj 
  | otherwise = rgbNew (colorDirecto + rgbNew colorIndirecto colorDirecto) $ brdf obj
  where
    rgbNew rgb0 rgb1 = formula rgb0 1 p p' norm' rgb1 True
    colorDirecto = luzDirecta nxtObj luz figuras True
    colorIndirecto = luzIndirecta nxtObj luz figuras gen' (n+1) nMx
    (gen',gen'') = split gen
    p' = (\(Obj _ _ point _ _ _) -> point) nxtObj
    norm' = (\(Obj _ _ _ normalObj _ _) -> normalObj) nxtObj

    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    nxtObj = objAleatorio figuras' obj gen gen'' -- Siguiente objeto que choca, q no sea el mismo
    
    objEsp = snd $ obtenerPrimeraColision $ map (oneCollision (Ray p (calcularDirEspejo w0 norm))) figuras' --Objeto que refleja el objeto



brdf :: Obj -> RGB
brdf (Obj rgb w0 p norm (kd,kr,ke) id )
  | kd == 1 = scale rgb
  | ke == 1 = RGB 1 1 1
  | otherwise = scale rgb

