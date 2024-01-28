{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module PhotonMap where

import Elem3D
    ( Foton(..),
      Luz(..),
      RGB(..),
      Ray(..),
      Point3D(..),
      movePoint,
      (#<),
      (#),
      pointDir,
      pointToPothon,
      (.*),
      modd,
      normal,
      modRGB,
      scale,
      addPoints,
      escalatePoint, Direction (Direction), distFot )
import Figuras
    ( getShapeID,
      oneCollision,
      Obj(..),
      Shape(Triangle),
      Triangulo(Triangulo) )
import System.Random (StdGen, split)
import Funciones
    ( objAleatorio,
      objEspejo,
      objCristal,
      mediaRGB,
      sumRGB,
      genPoint,
      genPointTotal,
      obtenerPrimeraColision,
      colision, fresnell,
      brdf, sumRGB, ruletaRusa,
      media, desviacionEstandar, fGaus, addNiebla, dirEspejo)
import Debug.Trace (trace)
import qualified Data.DList as DL
import Data.KdTree.Static ( kNearest, KdTree, inRadius, nearest)

import qualified Data.DList as DL
import qualified Data.Set as Set

-- | Función básica, genera una lista de fotones lanzados desde diferentes luces.
createPhoton :: Float -> DL.DList Foton -> Int -> Int -> Set.Set Shape -> [Luz] -> StdGen -> Int -> DL.DList Foton
createPhoton lzT fotones contador contMx figuras luces gen nRebotes
  | contador == contMx = fotones -- Devuelve la lista de fotones
  | contador == contMx `div` round (lzT / intLuz) = createPhoton (lzT - intLuz) fotones (contador+1) contMx figuras (tail luces) gen' nRebotes -- Cambio de luz
  | otherwise = createPhoton lzT newlisP (contador+1) contMx figuras luces gen' nRebotes -- Se vuelve a llamar con la nueva lista de fotones
  where
    (ray, Luz pointPapa rgbPadre intLuz) = selescLightSource luces contador contMx gen
    !newlisP = traceRay pointPapa ((4.0 * pi * intLuz) / fromIntegral contMx) rgbPadre fotones figuras nRebotes gen' nxtObj
    nxtObj = obtenerPrimeraColision $ Set.map (\figura -> oneCollision figura ray) figuras -- Siguiente objeto que choca

    gen' = snd $ split gen

-- | Función auxiliar que selecciona la luz de la que se va a lanzar el fotón.
{-# INLINE selescLightSource #-}
selescLightSource :: [Luz] -> Int -> Int -> StdGen -> (Ray, Luz)
selescLightSource luces contador contMx gen = (Ray luzP (movePoint (pointDir luzP) (genPointTotal gen) #< luzP), (Luz {..}))
  where
    (Luz {..}) = head luces

-- | Función auxiliar que calcula y almacena el recorrido de un fotón por la escena.
{-# INLINE traceRay #-}
traceRay :: Point3D -> Float -> RGB -> DL.DList Foton -> Set.Set Shape -> Int -> StdGen -> Obj -> DL.DList Foton
traceRay p pot rgb fotones figuras n gen obj
  | n == 0 || rgb == RGB 0 0 0 || mindObj obj < 0 = fotones
  | otherwise = result
  where
    result = case caso of
      0 -> photonD -- Difuso
      1 -> photonR --Refracción
      2 -> photonE -- Especular
      _ -> fotones -- Absorción

    pObj = colObj obj
    nObj = normObj obj

    (caso, por) = ruletaRusa (trObj obj) gen
    photonD = traceRay pObj (2*pi*pot'*por* abs (w0Obj nxtObj .* nObj)) (brdf obj figuras `modRGB` 255) (fotones `DL.snoc` foton) figuras (n-1) gen' nxtObj
    photonE = traceRay pObj (pot * por) (brdf obj figuras * rgb ) fotones figuras n gen' objEsp
    photonR = traceRay pObj (pot * por) (brdf obj figuras * rgb ) fotones figuras n gen' objCri

    foton = Foton pObj pot' rgb (w0Obj obj) (idObj obj)
    pot' = abs (w0Obj obj .* nObj)*pot / ((1+(modd (colObj obj #< p)/10.0))**2)
    
    nxtObj = objAleatorio figuras' obj gen -- Siguiente objeto que choca con dirección random
    objEsp = objEspejo figuras' (w0Obj obj) nObj pObj -- Siguiente objeto que choca con dirección espejo
    (objCri, _) = objCristal figuras (w0Obj obj) nObj 1 (reflObj obj) pObj -- Siguiente objeto que choca con dirección refracción

    figuras' = Set.filter (\shape -> idObj obj /= getShapeID shape) figuras -- Quita el objeto que choca de la lista de figuras para que al buscar la primera vez no choque consigo mismo

    gen' = snd $ split gen

-- | Función auxiliar que estima la densidad de fotones en un punto.
{-# INLINE estDensPhoton #-}
estDensPhoton :: [Foton] -> Obj -> Set.Set Shape -> Float -> RGB
estDensPhoton photons obj figuras radio = newRGB
  where
   -- newRGB = sumRGB $ map (\photon -> fusion obj (fGaus photons obj photon) photon) photons
    --newRGB = sumRGB $ map (\photon -> fusion obj (1/(radio * radio * pi)) photon) photons
    newRGB = sumRGB $ map (\photon -> fusion obj (1 / (1 + distFot (colObj obj) photon)) photon) photons
    fusion :: Obj -> Float -> Foton -> RGB
    fusion obj kernel fot = if dirFot fot .* w0Obj obj >= 0 then newRGB `modRGB` kernel else RGB 0 0 0
      where
        newRGB = modRGB (scale $ rgbFot fot) (iFot fot) * brdf obj figuras
        
-- photonMap :: KdTree Float Foton -> Int -> Set.Set Shape-> Obj -> RGB
-- photonMap kdt nPhoton figuras obj@(Obj rgb w0 p norm (kd,0,0) kr' id) = newRGB * RGB 255 255 255
--   where
--     !newRGB = kdToRGB kdt (round $ fromIntegral nPhoton * kd) figuras obj
--     figuras' = filter (\shape -> id /= getShapeID shape) figuras
--     objEsp = objEspejo figuras' w0 norm p
--     (objCri,_) = objCristal figuras' w0 norm 1 kr' p

-- photonMap kdt nPhoton figuras obj@(Obj rgb w0 p norm (kd,kr,ke) kr' id) = newRGB
--   where
--     !newRGB = kdToRGB kdt (round $ fromIntegral nPhoton * kd) figuras obj + (rgb * scale colorEsp) + rgb * scale colorCri
--     figuras' = filter (\shape -> id /= getShapeID shape) figuras
--     objEsp = objEspejo figuras' w0 norm p
--     (objCri,_) = objCristal figuras' w0 norm 1 kr' p
--     colorCri = if round (fromIntegral nPhoton * kr) == 0 then RGB 0 0 0 else photonMap kdt (round $ fromIntegral nPhoton * kr) figuras objCri --Fixear :D
--     colorEsp = if round (fromIntegral nPhoton * ke) == 0 then RGB 0 0 0 else photonMap kdt (round $ fromIntegral nPhoton * ke) figuras objEsp


-- kdToRGB :: KdTree Float Foton -> Int -> Set.Set Shape-> Obj -> RGB
-- kdToRGB kdt 0 figuras obj@(Obj rgb w0 p norm (kd,kr,ke) kr' id) = RGB 0 0 0
-- kdToRGB kdt nPhoton figuras obj@(Obj rgb w0 p norm (kd,kr,ke) kr' id) = newRGB
--   where
--     photons = kNearest kdt nPhoton (pointToPothon p)
--     photons' = filter (\(Foton point int dir rgbF idF) -> id == idF) photons
--     -- Coger solo fotones del mismo objeto
--     !newRGB = estDensPhoton photons' obj figuras

-- | Función principal que calcula el color de un punto a partir de un kdt de fotones.
photonMap :: KdTree Float Foton -> [Luz] -> Float -> Set.Set Shape -> Obj -> RGB
photonMap kdt luces radio figuras obj
  | mindObj obj < 0 = RGB 0 0 0
  | otherwise ={-  addNiebla luces obj 0.9 figuras $ -} difuso + espejo + cristal
  where
    fr = fresnell obj 1
    difuso = if kd == 0 then RGB 0 0 0 else kdToRGB kdt radio figuras obj
    espejo = if ke == 0 then RGB 0 0 0 else rgbObj obj * scale colorEsp --  `modRGB` ke
    cristal = if kr == 0 then RGB 0 0 0 else rgbObj obj * scale colorCri `modRGB` kr

    (kd,kr,ke) = trObj obj
    figuras' = Set.filter (\shape -> idObj obj /= getShapeID shape) figuras
    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj)
    (objCri,_) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj)

    colorCri = photonMap kdt luces radio figuras objCri
    colorEsp = photonMap kdt luces radio figuras objEsp

-- | Función auxiliar que calcula el color de un punto a partir de un kdt de fotones.
{-# INLINE kdToRGB #-}
kdToRGB :: KdTree Float Foton -> Float -> Set.Set Shape-> Obj -> RGB
kdToRGB kdt 0 figuras obj = RGB 0 0 0
kdToRGB kdt radio figuras obj = newRGB
  where
    photons = inRadius kdt radio (pointToPothon (colObj obj))
    -- photons = kNearest kdt (round radio) (pointToPothon (colObj obj)) -- Si quisieramos coger los k-fotones más cercanos
    photons' = filter (\fot -> idObj obj == idFot fot) photons -- Coger solo fotones del mismo objeto
    
    !newRGB = if null photons' then RGB 0 0 0 else estDensPhoton photons' obj figuras radio --Si no hay fotones el color es 0, sino ecuación de estiamcion de densidad