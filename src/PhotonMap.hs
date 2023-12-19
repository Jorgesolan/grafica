{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
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
      colision,
      brdf, sumRGB, ruletaRusa,gen2Point, mediaLRGB, objEspejoRandom, media, desviacionEstandar, fGaus, addNiebla )
import Debug.Trace (trace)
import Data.List ()
import Data.KdTree.Static ( kNearest, KdTree, inRadius, nearest )

createPhoton :: Float -> [Foton] -> Int -> Int -> [Shape] -> [Luz] -> StdGen -> Int -> [Foton]
createPhoton lzT fotones contador contMx figuras luces gen nRebotes
  | contador == contMx = fotones
  | contador == contMx `div` round (lzT / intLuz) = createPhoton (lzT-intLuz) fotones (contador+1) contMx figuras (tail luces) gen' nRebotes
  | otherwise = createPhoton lzT newlisP (contador+1) contMx figuras luces gen0' nRebotes
  where
    (gen', gen'') = split gen
    (gen0, gen0') = split gen''
    figLuz = filter (\shape -> 0 < getShapeID shape) figuras
    (ray, Luz pointPapa rgbPadre intLuz) = selescLightSource luces figLuz contador contMx gen gen0
    !newlisP = traceRay pointPapa (4.0*pi*intLuz/fromIntegral contMx) rgbPadre fotones figuras nRebotes gen' nxtObj
    nxtObj = snd $ obtenerPrimeraColision $ map (oneCollision ray) figuras -- Siguiente objeto que choca


selescLightSource :: [Luz] -> [Shape] -> Int -> Int -> StdGen -> StdGen -> (Ray, Luz)
selescLightSource luces figuras contador contMx gen gen' = (Ray pLuz (movePoint (pointDir pLuz) (genPointTotal gen gen') #< pLuz), luz) --(Ray (movePoint (Direction 0 (-1) 0) pRand) (movePoint (pointDir pRand) (genPoint gen gen') #< pRand), luz) 
  where
    luz@(Luz pLuz _ _) = head luces -- Tomar la primera luz 
    -- Contador que lleve photones, meteler formula y select de otra 
    -- (Triangle(Triangulo x y z rgb tupla kr id)) = if contador < contMx `div` 2 then head figuras else figuras !! 1
    -- (u,v) = gen2Point gen gen'
    -- pRand = randomTrianglePoint x y z u v
    -- luz = Luz pRand rgb 1
    -- randomTrianglePoint :: Point3D -> Point3D -> Point3D -> Float -> Float -> Point3D
    -- randomTrianglePoint a b c u v = addPoints a (addPoints (escalatePoint u (b # a)) (escalatePoint v (c # a)))

traceRay :: Point3D -> Float -> RGB -> [Foton] -> [Shape] -> Int -> StdGen -> Obj -> [Foton]
traceRay p pot rgb fotones figuras n gen obj
  | n == 0 = fotones
  | otherwise = result
  where
    result = if | caso == 0 -> photonD
                | caso == 1 -> photonR
                | caso == 2 -> photonE
                | rgb == RGB 0 0 0 -> fotones
                | otherwise -> fotones

    (caso, por) = ruletaRusa (trObj obj) gen'''

    photonD = traceRay (colObj obj) (2*pi*pot'*por* abs (w0' .* normObj obj)) (brdf obj `modRGB` 255) (fotones ++ [foton]) figuras (n-1) gen'''' nxtObj
    photonE = traceRay (colObj obj) (pot * por) (brdf obj * rgb ) fotones figuras n gen'''' objEsp -- De momento espejo blanco
    photonR = traceRay (colObj obj) (pot * por) (brdf obj * rgb ) fotones figuras n gen'''' objCri -- De momento cristal blanco

    foton = Foton (colObj obj) pot' rgb (idObj obj)
    pot' = abs (w0Obj obj .* normObj obj)*pot / ((1+(modd (colObj obj #< p)/50.0))**2)
    (gen', gen'') = split gen
    (gen''', gen'''') = split gen''
    w0' = w0Obj nxtObj
    nxtObj = objAleatorio figuras' obj gen gen'
    figuras' = filter (\shape -> idObj obj /= getShapeID shape) figuras

    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj)
    (objCri,_) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj)


photonMulToRGB :: [Foton] -> Obj -> [Shape] -> Float -> RGB
photonMulToRGB photons obj figuras radio = newRGB
  where
   -- newRGB = sumRGB $ map (\photon -> fusion obj (fGaus photons obj photon) photon) photons
    newRGB = sumRGB $ map (\photon -> fusion obj (1 / (1 + distFot (colObj obj) photon)) photon) photons
    fusion :: Obj -> Float -> Foton -> RGB
    fusion obj kernel fot = newRGB `modRGB` kernel
      where
        newRGB = modRGB (scale $ rgbFot fot) (iFot fot) * brdf obj
        figuras' = filter (\shape -> idFot fot /= getShapeID shape) figuras

-- photonMap :: KdTree Float Foton -> Int -> [Shape]-> Obj -> RGB
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


-- kdToRGB :: KdTree Float Foton -> Int -> [Shape]-> Obj -> RGB
-- kdToRGB kdt 0 figuras obj@(Obj rgb w0 p norm (kd,kr,ke) kr' id) = RGB 0 0 0
-- kdToRGB kdt nPhoton figuras obj@(Obj rgb w0 p norm (kd,kr,ke) kr' id) = newRGB
--   where
--     photons = kNearest kdt nPhoton (pointToPothon p)
--     photons' = filter (\(Foton point int dir rgbF idF) -> id == idF) photons
--     -- Coger solo fotones del mismo objeto
--     !newRGB = photonMulToRGB photons' obj figuras




photonMap :: KdTree Float Foton -> Float -> [Shape] -> StdGen -> Obj -> RGB
photonMap kdt radio figuras gen obj
  | kr == 0 && ke == 0 = addNiebla (Point3D 0 0 (-10)) obj 0.6 $ kdToRGB kdt (radio * kd) figuras obj
  -- | idObj obj == 0 = dielRGB--kr == 0 && kd > ke = dielRGB --Dielectrico lo tratamos especial :D
  | otherwise = addNiebla (Point3D 0 0 (-10)) obj 0.6 $ kdToRGB kdt (radio * kd) figuras obj + (rgbObj obj * scale colorEsp `modRGB` ke) + (rgbObj obj * scale colorCri `modRGB` kr)
  where

    (kd,kr,ke) = trObj obj
    figuras' = filter (\shape -> idObj obj /= getShapeID shape) figuras
    objEsp = objEspejo figuras' (w0Obj obj) (normObj obj) (colObj obj)
    (objCri,_) = objCristal figuras' (w0Obj obj) (normObj obj) 1 (reflObj obj) (colObj obj)
    
    colorCri = photonMap kdt radio figuras gen objCri
    colorEsp = photonMap kdt radio figuras gen objEsp
    porc = abs $ w0Obj obj .* normObj obj

    dielRGB = kdToRGB kdt radio figuras obj `modRGB` (porc*kd) + randEsp `modRGB` ((1-porc) * ke)
    randEsp = mediaRGB (colorEsp:espejosRand)
    espejosRand = map (photonMap kdt radio figuras gen) a
    (a,b) = unzip $ randObjEsp [] gen
    randObjEsp :: [Obj] -> StdGen -> [(Obj, StdGen)]
    randObjEsp objs gen
      | length objs == 6 = []
      | otherwise = (newObj, gen'):randObjEsp (newObj:objs) gen'
      where
        (newObj, gen') = objEspejoRandom figuras' (w0Obj obj) (normObj obj) (colObj obj) gen 0.1



kdToRGB :: KdTree Float Foton -> Float -> [Shape]-> Obj -> RGB
kdToRGB kdt 0 figuras obj = RGB 0 0 0
kdToRGB kdt radio figuras obj = newRGB
  where
    photons = inRadius kdt radio (pointToPothon (colObj obj))
    photons' = filter (\(Foton _ _ _ idF) -> idObj obj == idF) photons
    -- Coger solo fotones del mismo objeto
    !newRGB = if null photons' then RGB 0 0 0 else photonMulToRGB photons' obj figuras radio
