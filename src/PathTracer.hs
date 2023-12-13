{-# LANGUAGE BangPatterns #-}
module PathTracer where

import Elem3D ( Luz(..), RGB(..), divRGB, scale ) 
import Figuras ( Obj(..), Shape, getShapeID )
import System.Random (StdGen, split)
import Funciones
    ( objAleatorio, objEspejo, objCristal, formula, colision, brdf ) 
-- import Debug.Trace (trace)

pathTracer :: Float -> [Luz] -> [Shape] -> Float -> Float -> Obj -> StdGen -> RGB
pathTracer rFl luz !figuras !n nMx obj@(Obj _ _ _ _ _ _ id) gen
  | nMx == 0 = colorDirecto * RGB 255 255 255 -- Solo luz directa
  | otherwise = (colorDirecto + colorIndirecto) * RGB 255 255 255 -- Luz directa + indirecta
  where
    colorDirecto = luzDirecta rFl obj luz figuras True
    colorIndirecto = luzIndirecta rFl obj luz figuras gen (n+1) nMx

luzDirecta :: Float -> Obj -> [Luz] -> [Shape] -> Bool -> RGB
luzDirecta rFl obj luces figuras peso = case luces of
  []        -> RGB 0 0 0
  [x]       -> luzMono rFl obj x figuras peso
  (x:xs)    -> (luzMono rFl obj x figuras peso + luzDirecta rFl obj xs figuras peso) `divRGB` 2


luzMono :: Float -> Obj -> Luz -> [Shape] -> Bool -> RGB
luzMono rFl obj@(Obj rgb w0 p norm (kd,kr,ke) kr' id) (Luz pointLuz rgbLuz intLuz) figuras peso 
  | ke > 0 = luzMono rFl objEsp (Luz pointLuz rgbLuz intLuz) figuras True
  | kr > 0 = luzMono rFlNew objCr (Luz pointLuz rgbLuz intLuz) figuras True --Al hacer la colision, si es un obj de cristal el siguiente obj, seguir mirando en esa dir
  | otherwise = colorDirecto
  where
    colorDirecto = if colision p pointLuz figuras then newRGB else RGB 0 0 0
    newRGB = formula (scale rgbLuz) intLuz pointLuz p norm (brdf obj) True -- "Integral"
    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    objEsp = objEspejo figuras' w0 norm p
    (objCr, rFlNew) = objCristal figuras' w0 norm rFl kr p --ke por poner algo, ya se cambiara por la relfexion del obj

luzIndirecta :: Float -> Obj -> [Luz] -> [Shape] -> StdGen -> Float -> Float -> RGB
luzIndirecta rFl obj@(Obj _ w0 p norm (kd,kr,ke) kr' id) luz figuras gen n nMx 
  | kr > 0 = luzIndirecta rFlNew objCr luz figuras genxt n nMx -- Rayo reflejado
  | ke > 0 = luzIndirecta rFl objEsp luz figuras genxt n nMx
  | nMx == n = rgbNew colorDirecto $ brdf obj 
  | otherwise = rgbNew (colorDirecto + rgbNew colorIndirecto colorDirecto) $ brdf obj
  where
    rgbNew rgb0 rgb1 = formula rgb0 1 p p' norm' rgb1 True
    colorDirecto = luzDirecta rFl nxtObj luz figuras True
    colorIndirecto = luzIndirecta rFl nxtObj luz figuras genxt (n+1) nMx
    (gen',gen'') = split gen
    (genR, genxt) = split gen'
    p' = (\(Obj _ _ point _ _ _  _) -> point) nxtObj
    norm' = (\(Obj _ _ _ normalObj _ _ _) -> normalObj) nxtObj

    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    nxtObj = objAleatorio figuras' obj gen gen'' -- Siguiente objeto que choca, q no sea el mismo
    
    (objCr, rFlNew) = objCristal figuras' w0 norm rFl kr p
    objEsp = objEspejo figuras' w0 norm p --Objeto que refleja el objeto



