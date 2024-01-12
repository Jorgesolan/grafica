{-# LANGUAGE BangPatterns #-}

module PathTracer where

import Elem3D ( Luz(..), RGB(..), divRGB, scale, modRGB, (.*),
       Point3D (Point3D), normal,nanRGB )
import Figuras ( Obj(..), Shape, getShapeID )
import System.Random (StdGen, split)
import Funciones
    ( objAleatorio, objEspejo, objCristal, formula, 
    colision, brdf, ruletaRusa, addNiebla, dirEspejo)
import Debug.Trace (trace)
import qualified Data.Set as Set

{-# INLINE pathTracer #-}
pathTracer :: Float -> [Luz] -> Set.Set Shape -> Int -> Obj -> StdGen -> RGB
pathTracer rFl luz !figuras ppp obj gen
  | mindObj obj < 0 = scale $ RGB 20 50 30
  | ppp == 1 = colorIndirecto
  | otherwise = colorIndirecto + pathTracer rFl luz figuras (ppp - 1) obj gen' -- Luz directa + indirecta
  where
    colorIndirecto = luzIndirecta obj luz figuras gen''
    (gen',gen'') = split gen

{-# INLINE luzDirecta #-}
luzDirecta :: [Luz] -> Set.Set Shape -> Obj -> RGB
luzDirecta luces figuras obj
  | null luces = RGB 0 0 0
  | mindObj obj < 0 = scale $ RGB 20 40 50
  | length luces == 1 = {- addNiebla (head luces) obj 0.8 figuras $ -} luzMono obj (head luces) figuras
  | length  luces > 1 = (luzMono obj (head luces) figuras + luzDirecta (tail luces) figuras obj) `divRGB` 2 -- Si todas pesaran igual que no es asi

{-# INLINE luzMono #-}
luzMono :: Obj -> Luz -> Set.Set Shape -> RGB
luzMono obj (Luz pointLuz rgbLuz intLuz) figuras = difuso + espejo +cristal
  where
    (kd,ke,kr) = trObj obj

    newRGB = formula (scale rgbLuz) intLuz pointLuz (colObj obj) (normObj obj) (brdf obj figuras)

    figuras' = Set.filter (\shape -> idObj obj /= getShapeID shape) figuras
    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj)
    (objCr, _) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj)

    difuso = if colision (colObj obj) pointLuz figuras && kd > 0 then newRGB `modRGB` kd else RGB 0 0 0
    espejo = if kr > 0 then luzMono objEsp (Luz pointLuz rgbLuz intLuz) figuras `modRGB` kr else RGB 0 0 0
    cristal = if ke > 0 then luzMono objCr (Luz pointLuz rgbLuz intLuz) figuras `modRGB` ke else RGB 0 0 0

    --iridiscencia = iris obj

    -- wH = normal (dirEspejo (w0Obj obj) (normObj obj) - w0Obj obj)
    -- fres = fresnell obj 1
    -- micro = microfacet wH (normObj obj) 0.4
    -- shadow = shadowing (w0Obj obj) (normObj obj) 0.4

{-# INLINE luzIndirecta #-}
luzIndirecta :: Obj -> [Luz] -> Set.Set Shape -> StdGen -> RGB
luzIndirecta obj luz figuras gen = result where
    result = case caso of
        0 -> rgbNew rndObj (brdf obj figuras) `modRGB` por
        1 -> colorIndirecto objCr `modRGB` por
        2 -> colorIndirecto objEsp `modRGB` por
        _ -> RGB 0 0 0

    (caso, por) = ruletaRusa (trObj obj) gen

    colorDirecto nxtObj = luzDirecta luz figuras nxtObj `modRGB` abs ((w0Obj nxtObj .* normObj obj) * 2 * pi)

    colorIndirecto nxtObj = luzIndirecta nxtObj luz figuras gen'

    rgbNew nxtObj = formula (colorDirecto nxtObj + colorIndirecto nxtObj) 1 (colObj obj) (colObj nxtObj) (normObj nxtObj)

    figuras' = Set.filter (\shape -> idObj obj /= getShapeID shape) figuras
    rndObj = objAleatorio figuras' obj gen

    gen' = snd (split gen)

    (objCr, rFlNew) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj)
    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj) -- Multiplicarlo por el coseno de donde sale * 2pi por el monteCarlo

{-# INLINE luzArea #-}
luzArea :: Set.Set Shape -> Int -> Obj -> StdGen -> RGB
luzArea figuras 0 obj gen = luzAreaRec figuras obj gen
luzArea figuras p obj gen = luzArea figuras (p-1) obj gen' + luzAreaRec figuras obj gen''
  where
    (gen',gen'') =split gen

{-# INLINE luzAreaRec #-}
luzAreaRec :: Set.Set Shape -> Obj -> StdGen -> RGB
luzAreaRec figuras obj gen = rgbFin
  where
    rgbFin = if idObj obj == 4 then RGB 1 1 1 else if rgbObj rndObj == RGB 0 0 0 then RGB 0 0 0 else if nanRGB result then RGB 0 0 0 else result
    result = case caso of
        0 -> rgbNew rndObj (brdf obj figuras) `modRGB`(pi * por)
        1 -> luzAreaRec figuras objCr gen' `modRGB` por
        2 -> luzAreaRec figuras objEsp gen' `modRGB` por
        _ -> RGB 0 0 0
    
    (caso, por) = ruletaRusa (trObj obj) gen

    rgbNew nxtObj = formula (luzAreaRec figuras nxtObj gen') 1 (colObj obj) (colObj nxtObj) (normObj nxtObj)

    figuras' = Set.filter (\shape -> idObj obj /= getShapeID shape) figuras
    rndObj = objAleatorio figuras' obj gen

    gen' = snd (split gen)

    (objCr, rFlNew) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj)
    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj) -- Multiplicarlo por el coseno de donde sale * 2pi por el monteCarlo
