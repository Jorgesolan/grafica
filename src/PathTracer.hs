{-# LANGUAGE BangPatterns #-}
module PathTracer where

import Elem3D 
import Figuras
import System.Random (StdGen, split)
import Funciones 
import Debug.Trace (trace)

pathTracer :: Direction -> Obj -> Luz -> [Shape] -> StdGen -> Float -> Float -> RGB
pathTracer dir obj@(Obj rgb !p norm (kd,kr,ke) id) luz@(Luz !pointLuz rgbLuz intLuz) !figuras gen !n nMx
    -- | kr > 0 = pathTracer dir objCr luz figuras gen' n
  | nMx == 0 = colorDirecto * RGB 255 255 255 -- Si fin
  | otherwise = (colorDirecto + colorIndirecto) * RGB 255 255 255 -- Si llega luz a ese punto, no choca con otras figuras antes
   where
    colorDirecto = luzDirecta dir obj luz figuras True
    colorIndirecto = luzIndirecta dir obj nxtObj luz figuras gen' (n+1) nMx
    (gen',gen'') = split gen
    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    nxtObj = objAleatorio figuras' obj gen gen'' -- Siguiente objeto que choca, q no sea el mismo
    objEsp = snd $ obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray p (calcularDirEspejo dir norm))) figuras'

luzDirecta :: Direction -> Obj -> Luz -> [Shape] -> Bool -> RGB
luzDirecta dir obj@(Obj rgb p norm (kd,kr,ke) id) (Luz pointLuz rgbLuz intLuz) figuras peso 
  -- | ke > 0 = colorDirecto * luzDirecta dir objEsp (Luz pointLuz rgbLuz intLuz) figuras True
  | otherwise = colorDirecto
  where
    colorDirecto = if {- newIntLuz > 0 && -} (colision p pointLuz figuras) then newRGB else RGB 0 0 0
    newRGB = formula (scale rgbLuz) intLuz pointLuz p norm (scale $ brdf obj) True -- "Integral"
    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    objEsp = snd $ obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray p (calcularDirEspejo dir norm))) figuras'


luzIndirecta :: Direction -> Obj -> Obj -> Luz -> [Shape] -> StdGen -> Float -> Float -> RGB
luzIndirecta dir objBRDF@(Obj _ p0 norm _ _) obj@(Obj _ p _ (kd,kr,ke) id) luz figuras gen n nMx 
  -- | ke > 0 = colorDirecto * luzIndirecta dir objBRDF objEsp luz figuras gen' n nMx
  | nMx == n = formula colorDirecto 1 p p0 norm (scale $ brdf objBRDF) True
  | otherwise = formula (colorDirecto + colorIndirecto) 1 p p0 norm (scale $ brdf objBRDF) True 
  where
    colorDirecto = luzDirecta dir obj luz figuras True
    colorIndirecto = luzIndirecta dir objBRDF nxtObj luz figuras gen' (n+1) nMx
    (gen',gen'') = split gen
    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    nxtObj = objAleatorio figuras' obj gen gen'' -- Siguiente objeto que choca, q no sea el mismo
    
    objEsp = snd $ obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray p (calcularDirEspejo dir norm))) figuras'

--Separa Directa e indirecta, indirecta recursiva

-- fusionLus :: Obj -> Luz -> RGB
-- fusionLus (Obj rgb p norm _ _) (Luz pointLuz rgbLuz intLuz) = newRGB
--   where
--     newRGB = prodRGB rgbLuz rgb (if newIntLuz > 1 then 1 else newIntLuz)
--     newIntLuz = formula 10 p pointLuz norm (16/pi) False -- "Integral"


-- calcLuz :: Direction -> Obj -> Luz -> [Shape] -> StdGen -> Float -> Luz
-- calcLuz dir obj@(Obj rgb p norm (kd,kr,ke) id) luz@(Luz pointLuz rgbLuz intLuz) figuras gen n
--   | n == 0 = luzDirecta -- Si fin
--   | otherwise = Luz p (colorDirecto + newColor) (newIntLuz+intLuznew)
--   where
--     colorDirecto = (\(Luz _ rgb _) -> rgb) luzDirecta
--     luzDirecta = if colision p pointLuz figuras then Luz p newRGB newIntLuz else Luz p (RGB 0 0 0) 0
--     newRGB = prodRGB rgbLuz rgb newIntLuz
--     newIntLuz = formula intLuz p pointLuz norm (4/pi) False-- "Integral"

--     intLuznew = (\(Luz _ _ int) -> int) newLuz
--     newColor = fusionLus obj newLuz 
--     (gen',gen'') = split gen
--     newLuz = calcLuz dir nxtObj luz figuras gen' (n-1)
--     figuras' = filter (\shape -> id /= getShapeID shape) figuras
--     nxtObj = objAleatorio figuras' obj gen gen'' -- Siguiente objeto que choca, q no sea el mismo


-- -- nxtObjetito :: a -> [Shape] -> Point3D -> Obj
-- -- nxtObjetito a fig p = snd $ obtenerPrimeraColision $ map (\figura -> oneCollision figura (Ray p a )) fig

