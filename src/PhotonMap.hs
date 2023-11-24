{-# LANGUAGE BangPatterns #-}
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
createPhoton pot fotones cont figuras luces gen =
  createPhoton pot newlisP (cont-1) figuras luces gen0'
  where
    (gen', gen'') = split gen
    (gen0, gen0') = split gen''
    (ray, (Luz pointPapa rgbPadre potfpadre)) = selescLightSource luces gen gen0
    newlisP = traceRay pot rgbPadre fotones figuras ray 4 gen' (-1)
   

selescLightSource :: [Luz] -> StdGen -> StdGen -> (Ray, Luz)
selescLightSource luces gen gen' = (Ray pLuz ((movePoint (pointDir pLuz) (genPointTotal gen gen')) #< pLuz), luz)
  where
    luz = head luces -- Tomar la primera luz (cambiar si es necesario)
    pLuz = ((\(Luz pLuz _ _) -> pLuz) luz)

traceRay :: Float -> RGB -> [Foton] -> [Shape] -> Ray -> Int -> StdGen -> Int -> [Foton]
traceRay pot rgb fotones figuras (Ray p dir) n gen id
  | n == 0 = fotones
  | otherwise = traceRay (pot/2) rgb' (fotones ++ [foton]) figuras (Ray point newDir) (n-1) gen'' id'
  where
    figuras' = filter (\shape -> id /= getShapeID shape) figuras
    foton = Foton point pot' dir rgb id'
    pot' = (pot / ((1+(modd (point #< p)/25.0))**2))
    point = obtenerPunto obj
    normalObj = (\(Obj _ _ _ normalObj _ _) -> normalObj) obj
    id' = (\(Obj _ _ _ _ _ id') -> id') obj 
    rgb' = (\(Obj rgb _ _ _ _ _) -> rgb) obj
    (gen', gen'') = split gen
    obj = snd $ obtenerPrimeraColision $ map (oneCollision (Ray p dir)) figuras'
    
    newDir = normal $ puntoAl #< p
    puntoAl = cambioBase p (generateBase dirAl normalObj (normal (dirAl * normalObj))) $ genPoint gen gen'
    dirAl = normal $ normalObj * Direction 2 1 (-2)



photonMulToRGB :: [Foton] -> Obj -> [Shape] -> RGB
photonMulToRGB photons obj@(Obj rgb w0 p norm _ _) figuras = newRGB
  where
    newRGB = mediaRGB $ map (fusion obj) photons
    fusion :: Obj-> Foton -> RGB
    fusion obj@(Obj rgb w0 p norm _ id) (Foton point int dir rgbF idF) = if colision p point figuras' || id == idF then newRGB else RGB 0 0 0
      where
        newRGB = (modRGB (scale rgbF) $ int / ((1+(modd (point #< p)))**2)) * (brdf obj) `modRGB` dotP
        dotP = if (normal dir .* normal norm) < 0 then abs ((normal norm) .* (normal dir)) else 0
        figuras' = filter (\shape -> idF /= getShapeID shape) figuras

photonMap :: KdTree Float Foton -> Int -> [Shape]-> Obj -> RGB
photonMap kdt nPhoton figuras obj@(Obj rgb w0 p norm _ _) = newRGB * RGB 255 255 255
  where
    newP = pointToPothon p
    photons = kNearest kdt nPhoton newP
    newRGB = photonMulToRGB photons obj figuras




