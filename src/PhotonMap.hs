{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module PhotonMap where


import Elem3D 
import Figuras
import System.Random (StdGen, split)
import Funciones 
import Debug.Trace (trace)
import Data.List
import Data.KdTree.Static 

createPhoton :: Float -> [Foton] -> Float -> [Shape] -> [Luz] -> StdGen -> [Foton]
createPhoton pot fotones 0 _ _ _ = fotones
createPhoton pot fotones cont figuras luces gen = createPhoton pot newlisP (cont-1) figuras luces gen0'
  where
    (gen', gen'') = split gen
    (gen0, gen0') = split gen''
    (ray, (Luz pointPapa rgbPadre _)) = selescLightSource luces gen gen0
    newlisP = traceRay pointPapa pot rgbPadre fotones figuras 1 gen' nxtObj
    nxtObj = snd $ obtenerPrimeraColision $ map (oneCollision ray) figuras -- Siguiente objeto que choca
   

selescLightSource :: [Luz] -> StdGen -> StdGen -> (Ray, Luz)
selescLightSource luces gen gen' = (Ray pLuz ((movePoint (pointDir pLuz) (genPointTotal gen gen')) #< pLuz), luz)
  where
    luz@(Luz pLuz _ _) = head luces -- Tomar la primera luz (cambiar si es necesario)

traceRay :: Point3D -> Float -> RGB -> [Foton] -> [Shape] -> Int -> StdGen -> Obj -> [Foton]
traceRay p pot rgb fotones figuras n gen obj@(Obj rgb' w0 point normalObj (kd,kr,ke) id)
  | n == 0 = fotones
  | otherwise = result
  where
    result = if | ke > 0 -> photonE 
                | kr > 0 -> photonR
                | otherwise -> photonD


    photonD = traceRay point (pot'/2) rgb' (fotones ++ [foton]) figuras (n-1) gen'' nxtObj
    photonE = traceRay point pot' rgb fotones figuras n gen'' objEsp -- De momento espejo blanco
    photonR = traceRay point pot' rgb fotones figuras n gen'' objCri -- De momento cristal blanco

    foton = Foton point pot' w0 rgb id
    pot' = (pot / ((1+(modd (point #< p)/25.0))**2))
    (gen', gen'') = split gen
    nxtObj = objAleatorio figuras' obj gen gen'
    figuras' = filter (\shape -> id /= getShapeID shape) figuras

    objEsp = objEspejo figuras' w0 normalObj point
    (objCri,_) = objCristal figuras' w0 normalObj 1 kr point



photonMulToRGB :: [Foton] -> Obj -> [Shape] -> RGB
photonMulToRGB photons obj@(Obj rgb w0 p norm _ _) figuras = newRGB
  where
    newRGB = mediaRGB $ map (fusion obj) photons
    fusion :: Obj-> Foton -> RGB
    fusion obj@(Obj rgb w0 p norm (kd,kr,ke) id) (Foton point int dir rgbF idF) = if colision p point figuras' || id == idF then newRGB else RGB 0 0 0
      where
        newRGB = (modRGB (scale rgbF) $ int / ((1+(modd (point #< p)))**2)) * (brdf obj) `modRGB` dotP
        dotP = if (normal dir .* normal norm) < 0 then abs ((normal norm) .* (normal dir)) else 0
        figuras' = filter (\shape -> idF /= getShapeID shape) figuras

photonMap :: KdTree Float Foton -> Int -> [Shape]-> Obj -> RGB
photonMap kdt nPhoton figuras obj@(Obj rgb w0 p norm (kd,kr,ke) id)
  | kr > 0 = photonMap kdt nPhoton figuras objCri
  | ke > 0 = photonMap kdt nPhoton figuras objEsp
  | otherwise = newRGB * RGB 255 255 255
  where
    photons = kNearest kdt nPhoton (pointToPothon p)
    newRGB = photonMulToRGB photons obj figuras

    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    objEsp = objEspejo figuras' w0 norm p
    (objCri,_) = objCristal figuras' w0 norm 1 kr p




