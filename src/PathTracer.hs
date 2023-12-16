{-# LANGUAGE BangPatterns #-}
module PathTracer where

import Elem3D ( Luz(..), RGB(..), divRGB, scale, modRGB )
import Figuras ( Obj(..), Shape, getShapeID )
import System.Random (StdGen, split)
import Funciones
    ( objAleatorio, objEspejo, objCristal, formula, colision, brdf )

pathTracer :: Float -> [Luz] -> [Shape] -> Float -> Float -> Obj -> StdGen -> RGB
pathTracer rFl luz !figuras !n nMx obj gen
  | nMx == 0 = colorDirecto * RGB 255 255 255 -- Solo luz directa
  | otherwise = (colorDirecto + colorIndirecto) * RGB 255 255 255 -- Luz directa + indirecta
  where
    colorDirecto = luzDirecta obj luz figuras True
    colorIndirecto = luzIndirecta obj luz figuras gen (n+1) nMx

luzDirecta :: Obj -> [Luz] -> [Shape] -> Bool -> RGB
luzDirecta obj luces figuras peso 
  | null luces = RGB 0 0 0
  | length luces == 1 = luzMono obj (head luces) figuras kd peso
  | length  luces > 1 = (luzMono obj (head luces) figuras kd peso + luzDirecta obj (tail luces) figuras peso) `divRGB` 2
  where
    (kd,_,_) = trObj obj

luzMono :: Obj -> Luz -> [Shape] -> Float -> Bool -> RGB
luzMono obj (Luz pointLuz rgbLuz intLuz) figuras peso bool
  | intLuz * peso < 0.05 = RGB 0 0 0
  | otherwise = colorDirecto `modRGB` kd + luzMono objEsp (Luz pointLuz rgbLuz intLuz) figuras kr bool `modRGB` kr + luzMono objCr (Luz pointLuz rgbLuz intLuz) figuras ke bool `modRGB` ke
  where
    (kd,ke,kr) = trObj obj
    colorDirecto = if colision (colObj obj) pointLuz figuras then newRGB else RGB 0 0 0
    newRGB = formula (scale rgbLuz) intLuz pointLuz (colObj obj) (normObj obj) (brdf obj) True -- "Integral"
    figuras' = filter (\shape -> idObj obj /= getShapeID shape) figuras
    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj)
    (objCr, _) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj) --ke por poner algo, ya se cambiara por la relfexion del obj

luzIndirecta :: Obj -> [Luz] -> [Shape] -> StdGen -> Float -> Float -> RGB
luzIndirecta obj luz figuras gen n nMx
  -- | kr > 0 = luzIndirecta rFlNew objCr luz figuras genxt n nMx -- Rayo reflejado
  -- | ke > 0 = luzIndirecta rFl objEsp luz figuras genxt n nMx
  | nMx == n = rgbNew colorDirecto $ brdf obj
  | otherwise = rgbNew (colorDirecto + rgbNew colorIndirecto colorDirecto) $ brdf obj
  where
    rgbNew rgb0 rgb1 = formula rgb0 1 (colObj obj) p' norm' rgb1 True
    colorDirecto = luzDirecta nxtObj luz figuras True
    colorIndirecto = luzIndirecta nxtObj luz figuras genxt (n+1) nMx
    (gen',gen'') = split gen
    (genR, genxt) = split gen'
    p' = colObj nxtObj
    norm' = normObj nxtObj

    figuras' = filter (\shape -> idObj obj /= getShapeID shape) figuras
    nxtObj = objAleatorio figuras' obj gen gen'' -- Siguiente objeto que choca, q no sea el mismo

    (objCr, rFlNew) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj)
    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj) --Objeto que refleja el objeto



