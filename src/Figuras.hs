{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Figuras where
import Elem3D
    ( RGB(..),
      Base,
      Ray(..),
      Direction(..),
      Point3D(..),
      movePoint,
      (#<),
      (#),
      (.*),
      escalateDir, modd,
      normal, escalatePoint, pointDir, dirPoint, distPoint, addPoints )
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

import qualified Data.DList as DL
import qualified Data.Set as Set

-- | Tipo de dato básico, se usa para representar las coordenadas U V de texturas.
data Point2D = Point2D {uP :: Float, vP :: Float} deriving Show

-- |Tipo compuesto, este representa la base de la camara y su posición tridimensional sobre dicha base.
data Camara = Camara Point3D Base
-- |Tipo compuesto, representa una esfera, tiene un punto central, la longitud del radio, el color, las propiedades del material y su indice de reflexión.
data Esfera = Esfera {centEs :: Point3D, radEs :: Float, rgbEs ::  RGB, trEs :: (Float, Float, Float), reflEs :: Float, idEs :: Int} deriving Show
-- |Tipo compuesto, representa un plano, tiene un punto central, la dirección normal al plano, el color, las propiedades del material y su indice de reflexión.
data Plano = Plano {centPl :: Point3D, normPl :: Direction, rgbPl :: RGB, trPl :: (Float, Float, Float), reflPl :: Float, idPl :: Int} deriving Show
-- |Tipo compuesto, representa un triángulo, tiene sus 3 vertices, el color, las propiedades del material y su indice de reflexión.
data Triangulo = Triangulo {p0Tr :: Point3D, p1Tr :: Point3D, p2Tr :: Point3D,uv0Tr :: Point2D, uv1Tr :: Point2D, uv2Tr :: Point2D, rgbTr :: RGB, trTr :: (Float, Float, Float), reflTr :: Float, idTr :: Int} deriving Show
-- |Tipo compuesto, representa un cilindro, tiene un punto central, la longitud del radio, el color, las propiedades del material y su indice de reflexión.
data Cilindro = Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int deriving Show
-- |Tipo compuesto, representa un cono, tiene un punto central, la longitud del radio, el color, las propiedades del material y su indice de reflexión.
data Cono = Cono {centCo :: Point3D, altCo :: Float, radCo :: Float, rgbCo :: RGB, trCo :: (Float, Float, Float), reflCo :: Float, idCo :: Int} deriving Show
-- |Tipo compuesto, representa un rectángulo, tiene un punto central, las direcciones normal y tangente, longitud y anchura, el color, las propiedades del material y su indice de reflexión.
data Rectangulo = Rectangulo {centRe :: Point3D, normRe :: Direction, tngRe :: Direction, altRe :: Float, ancRe :: Float, rgbRe :: RGB, trRe :: (Float, Float, Float), reflRe:: Float, idRe:: Int} deriving Show


-- |Tipo compuesto, representa una hitbox 3D de forma rectangular, tiene dos puntos que representan los vertices de cada extremo.
data AABB = AABB {p0AB :: Point3D, p1AB ::  Point3D} deriving Show

-- |Tipo compuesto, representa una BVH, es un tipo especial dado que es recursivo, contiene un AABB que actua como Hitbox, subinstancias de si mismo y una lista de triángulos.
data BVH = BVH {aabb::AABB, bvhs :: [BVH], triangulos :: [Triangulo], idBvh :: Int} deriving Show

-- |Tipo auxiliar, representa la posición de un triángulo, solo tiene sus 3 vértices.
data TrianglePos = TrianglePos { v1 :: Int, v2 :: Int, v3 :: Int } deriving Show


-- |Tipo especial, sería lo equivalente a una clase virtual, esta nos permite interactuar de forma transparente con su contenido sin necesidad de saber la clase concreta que contiene.
data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo | Cylinder Cilindro | Rectangle Rectangulo | Acelerator BVH | Cone Cono | Null deriving Show


-- |Tipo compuesto, contiene todas las propiedades obtenidas al colisionar un rayo con un objeto, la distancia del impacto, el color, la dirección incidente, el punto de colisión, la dirección normal de ese punto para el objeto y las propiedades internas del mismo(coeficientes).
data Obj = Obj {mindObj :: Float, rgbObj :: RGB, w0Obj :: Direction, colObj :: Point3D, normObj :: Direction, trObj ::(Float, Float, Float), reflObj :: Float, idObj:: Int,shObj :: Shape} deriving Show

instance Eq Shape where
    a == b = False
instance Ord Shape where
    compare = comparing getShapeID

instance Eq Obj where
    obj == obj1 = mindObj obj == mindObj obj1
instance Ord Obj where
    compare obj obj1 = compare (mindObj obj) (mindObj obj1)

-- |Función auxiliar, para dada una SHAPE y un punto3d de la misma, devolver en coordenadas UV la posición de dicho punto.(Se emplea para las texturas)
{-# INLINE getUV #-}
getUV :: Shape -> Point3D -> (Float, Float)
--getUV (Plane (Plano {..})) p = (1,1)
getUV (Sphere (Esfera {..})) p = (u,v)
    where
        (Point3D x y z) = p
        (Point3D cx cy cz) = centEs
        u = 0.5 + atan2 (z - cz) (x - cx) / (2 * pi)
        v = 0.5 - asin ((y - cy) / radEs) / pi

getUV (Triangle (Triangulo {..})) p = (u,v)
    where
        v0 =  p2Tr #< p0Tr
        v1 =  p1Tr #< p0Tr
        v2 =  p #< p0Tr
        n = v1 * v0
        cross1 = v2 * v1
        cross2 = v0 * v2
        aTri = 0.5 * modd n
        alpha = modd cross1 / (2 * aTri)
        beta = modd cross2 / (2 * aTri)
        gamma = 1 - alpha - beta
        u = (alpha * uP uv0Tr + beta * uP uv1Tr + gamma * uP uv2Tr) 
        v = (alpha * vP uv0Tr + beta * vP uv1Tr + gamma * vP uv2Tr) 

        
        
getUV (Cylinder (Cilindro p1 p2 p3 _ _ _ _)) p = (1,1)
getUV (Rectangle(Rectangulo {..})) p = (u,v)
    where
      halfHeight = altRe / 2
      halfWidth = ancRe / 2
      Direction x y z = normRe 
      right = tngRe
      up = normal $ normRe * normal right
      bottomLeft = calculateVertex (-halfWidth) (-halfHeight)
      bottomRight = calculateVertex halfWidth (-halfHeight)
      topLeft = calculateVertex (-halfWidth) halfHeight

      calculateVertex w h = centRe `addPoints` dirPoint (escalateDir w right + escalateDir h up)

      !u = distanceToRay p (Ray bottomLeft (bottomLeft #< topLeft)) / altRe
      !v = distanceToRay p (Ray bottomLeft (bottomLeft #< bottomRight)) / ancRe

      distanceToRay :: Point3D -> Ray -> Float -- Punto más cercano al rayo
      distanceToRay point ray =
        let
            (Point3D ox oy oz) = oR ray
            (Direction dx dy dz) = dR ray
            px = ox - xP point
            py = oy - yP point
            pz = oz - zP point
            a = dx * dx + dy * dy + dz * dz
            b = px * dx + py * dy + pz * dz
            t = -(b / a)
            closestPoint = Point3D (ox + t * dx) (oy + t * dy) (oz + t * dz)
            in distPoint point closestPoint

        -- (Direction x y z) = normal normRe
        -- pAbI = Point3D (xP centRe - (ancRe/2 * abs(1 - x))) (yP centRe - (altRe/2 * abs(1 - y))) (zP centRe + (ancRe/2 * abs(1 - z)))
        -- pArI = Point3D (xP centRe - (ancRe/2 * abs(1 - x))) (yP centRe - (altRe/2 * abs(1 - y))) (zP centRe - (ancRe/2 * abs(1 - z)))
        -- pAbD = Point3D (xP centRe - (ancRe/2 * abs(1 - x))) (yP centRe - (altRe/2 * abs(1 - y))) (zP centRe - (ancRe/2 * abs(1 - z)))
    
getUV (Acelerator (BVH {..})) p = (1,1)


-- |Función auxiliar, dada una lista de triángulos, calcula su hitbox y la devuelve.
{-# INLINE calculateBoundingBox #-}
calculateBoundingBox :: [Triangulo] -> AABB
calculateBoundingBox triangles =
    let xs = [x | tri <- triangles, x <- [xP (p0Tr tri), xP (p1Tr tri), xP (p2Tr tri)]]
        ys = [y | tri <- triangles, y <- [yP (p0Tr tri), yP (p1Tr tri), yP (p2Tr tri)]]
        zs = [z | tri <- triangles, z <- [zP (p0Tr tri), zP (p1Tr tri), zP (p2Tr tri)]]
        minPoint = Point3D (minimum xs) (minimum ys) (minimum zs)
        maxPoint = Point3D (maximum xs) (maximum ys) (maximum zs)
    in AABB minPoint maxPoint

-- |Función auxiliar, dado un entero y una lista de triángulos, genera una lista de listas de triángulos, cada sublista tiene tantos triángulos como indique el entero.
{-# INLINE splitTriangles #-}
splitTriangles :: Int -> [Triangulo] -> [[Triangulo]]
splitTriangles maxSize [] = []
splitTriangles maxSize triangles =
    let (first, rest) = splitAt maxSize triangles
    in first : splitTriangles maxSize rest

-- |Función básica, dada una lista de triángulos genera un BVH a partir de esta.
{-# INLINE buildBVH #-}
buildBVH :: Int -> [Triangulo] -> BVH
buildBVH idNum triangles =
    let bbox = calculateBoundingBox triangles
        maxSize = 64
    in if length triangles <= maxSize
        then BVH bbox [] triangles idNum
        else
            let sublists = splitTriangles (length triangles `div` maxSize) triangles
                subBVHs = zipWith (\idx sublist -> buildBVH idx sublist) [idNum..] sublists
            in BVH bbox (subBVHs) [] idNum

-- |Función auxiliar, dado un rayo y una Hitbox comprueba si estos colisionan o no, devuelve un booleano para indicarlo.
{-# INLINE rayIntersectsAABB #-}
rayIntersectsAABB :: Ray -> AABB -> Bool
rayIntersectsAABB (Ray {oR = Point3D x y z ,dR = Direction dx dy dz}) (AABB {p0AB = Point3D minx miny minz, p1AB = Point3D maxx maxy maxz}) =
    let tx1 = (minx - x) / dx
        tx2 = (maxx - x) / dx
        ty1 = (miny - y) / dy
        ty2 = (maxy - y) / dy
        tz1 = (minz - z) / dz
        tz2 = (maxz - z) / dz
        tmin = maximum [min tx1 tx2, min ty1 ty2, min tz1 tz2]
        tmax = minimum [max tx1 tx2, max ty1 ty2, max tz1 tz2]
    in tmax >= tmin && tmax >= 0

-- |Función auxiliar, dado un rayo y una lista de triángulos, intersecta este con todos y devuelve la intersección más cercana.
{-# INLINE closestIntersection #-}
closestIntersection :: Ray -> [Triangulo] -> (Float, Triangulo)
closestIntersection _ [] = (-1, Triangulo (Point3D 0 0 0) (Point3D 0 0 0) (Point3D 0 0 0) (Point2D 0 0) (Point2D 0 0) (Point2D 0 0) (RGB 0 0 0) (0, 0, 0) 0 0) -- Default value
closestIntersection (Ray {..}) triangles =
    foldr findClosestIntersection (-1, head triangles) triangles
    where
        findClosestIntersection :: Triangulo -> (Float, Triangulo) -> (Float, Triangulo)
        findClosestIntersection triangle (minDist, closestTri) =
            let intersection = ray1TriangleIntersection oR dR (p0Tr triangle) (p1Tr triangle) (p2Tr triangle)
            in case intersection of
                Just (t, _) ->
                    if minDist < 0 || (t > 0 && t < minDist)
                        then (t, triangle)
                        else (minDist, closestTri)
                Nothing -> (minDist, closestTri)


-- |Función auxiliar, dada una figura individual, añade esta a una lista de figuras.
addFig :: Shape -> [Shape] -> [Shape]
addFig (Plane (Plano {..})) shapes = Plane (Plano centPl normPl rgbPl trPl reflPl (length shapes)):shapes
addFig (Sphere (Esfera {..})) shapes = Sphere (Esfera centEs radEs rgbEs trEs reflEs (length shapes)):shapes
addFig (Triangle (Triangulo {..})) shapes = Triangle (Triangulo p0Tr p1Tr p2Tr uv0Tr uv1Tr uv2Tr rgbTr trTr reflTr (length shapes)):shapes
addFig (Cylinder (Cilindro p1 p2 p3 color reflec kr _)) shapes = Cylinder (Cilindro p1 p2 p3 color reflec kr (length shapes)):shapes
addFig (Rectangle(Rectangulo {..})) shapes = Rectangle (Rectangulo centRe normRe tngRe altRe ancRe rgbRe trRe reflRe (length shapes)):shapes
addFig (Acelerator (BVH {..})) shapes = Acelerator (BVH aabb bvhs triangulos (length shapes)):shapes
addFig (Cone (Cono {..})) shapes = Cone (Cono centCo altCo radCo rgbCo trCo reflCo (length shapes)):shapes

-- |Función básica, junta 2 listas de figuras.
addFigMult :: [Shape] -> [Shape] -> [Shape]
addFigMult xs shapes = foldl (flip addFig) shapes xs

-- |Función básica, convierte las figuras en luces de área.
encenderShapes :: [Shape] -> [Shape]
encenderShapes = map encenderShape

-- |Función auxiliar, convierte una figura a luz de área.
encenderShape :: Shape -> Shape
encenderShape (Plane (Plano {..})) = Plane (Plano centPl normPl rgbPl trPl reflPl (-idPl))
encenderShape (Sphere (Esfera {..})) = Sphere (Esfera centEs radEs rgbEs trEs reflEs (-idEs))
encenderShape (Triangle (Triangulo {..})) = Triangle (Triangulo p0Tr p1Tr p2Tr uv0Tr uv1Tr uv2Tr rgbTr trTr reflTr (-idTr))

{-# INLINE parametricShapeCollision #-}
-- |Función básica, dada una figuro y una lista de rayos, devuelve la lista de colisiones de cada uno de los rayos con la figura.
parametricShapeCollision :: Set.Set Shape -> [Ray] -> [Set.Set Obj]
parametricShapeCollision shapes rays = map (collision shapes) rays
  where
    collision shapes ray = Set.map (`oneCollision` ray) shapes

{-# INLINE oneCollision #-}
oneCollision :: Shape -> Ray -> Obj
-- |Función auxiliar, dada una figura y un rayo traza la colisión.
oneCollision es@(Sphere (Esfera {..})) (Ray {..}) =
    let f = oR #< centEs
        a = dR .* dR
        b = 2.0 * (f .* dR)
        c = f .* f - radEs * radEs
        raiz = b * b - 4.0*a*c
    in
        (if raiz > 0 then (let t0 = (-b + sqrt raiz) / (2.0 * a)
                               t1 = (-b - sqrt raiz) / (2.0 * a)
                               mind = findMinPositive t0 t1
                               collisionPoint = movePoint (escalateDir mind dR) oR
                               vectorNormal = normal $ collisionPoint #< centEs
                           in if t0 > 0 || t1 > 0
                               then (Obj mind rgbEs dR collisionPoint vectorNormal trEs reflEs idEs es)
                               else (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) trEs reflEs 0 es)) else (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) trEs reflEs 0 es))

oneCollision pl@(Plane (Plano {..})) (Ray {..}) = (Obj mind rgbPl dR collisionPoint vectorNormal trPl reflPl idPl pl)
  where
    mind = ((centPl #< oR) .* vectorNormal) / (dR .* vectorNormal)
    collisionPoint = movePoint (escalateDir mind dR) oR
    vectorNormal = if (dR.*normPl) > 0 then normal (escalateDir (-1) normPl) else normal normPl


oneCollision cl@(Cylinder(Cilindro p0 n r color reflec kr id)) (Ray {..}) = (Obj mind color dR collisionPoint vectorNormal reflec kr id cl)
  where
    mind = findMinPositive t1 t2
    t1 = ((p0 #< oR) .* vectorNormal + sqrt discriminant) / (dR .* vectorNormal)
    t2 = ((p0 #< oR) .* vectorNormal - sqrt discriminant) / (dR .* vectorNormal)
    discriminant = ((oR #< p0) .* (oR #< p0)) - r * r
    collisionPoint = movePoint (escalateDir mind dR) oR
    vectorNormal = if (dR.*n)>0 then normal (escalateDir (-1) n) else normal n


oneCollision acl@(Acelerator (BVH {..})) ray =
    if rayIntersectsAABB ray aabb
        then
            if null bvhs
                then
                    oneCollision (Triangle $ snd(closestIntersection ray triangulos)) ray
                else
                    let childCollisions = filter (\obj -> mindObj obj /= -1) $ map (\ch -> oneCollision (Acelerator ch) ray ) bvhs

                    in case childCollisions of
                        [] -> (Obj (-1) (RGB 0 0 0) (dR ray) (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0 acl)
                        _ -> let mindObj = minimum childCollisions
                             in mindObj
        else (Obj (-1) (RGB 0 0 0) (dR ray) (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0 acl)


oneCollision  rct@(Rectangle (Rectangulo {..})) (Ray {..})
  | denom /= 0 && t > 0 && withinBounds = (Obj t rgbRe dR (dirPoint collisionPoint) normRe' trRe reflRe idRe rct)
  | otherwise = (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) trRe reflRe 0 rct )
  where
    offset = collisionPoint - pointDir centRe
    localX = offset .* right
    localY = offset .* up
    halfWidth = ancRe / 2
    halfHeight = altRe / 2
    collisionPoint = pointDir oR + escalateDir t dR
    t = (centRe #< oR) .* normRe / denom
    withinBounds = -halfWidth <= localX && localX <= halfWidth && -halfHeight <= localY && localY <= halfHeight
    denom = dR .* normRe
    right = normal tngRe
    up = normRe * right
    normRe' = if dR .* normRe > 0 then normal (escalateDir (-1) normRe) else normal normRe

oneCollision cn@(Cone (Cono{..})) (Ray{..})
  | denom < 0  || t < 0 = (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0 cn)
  | otherwise = trace (show t ++ " " ++ show collisionPoint) $ (Obj t rgbCo dR collisionPoint normCo trCo reflCo idCo cn)
    where
        t = findMinPositive t1 t2
        collisionPoint = movePoint (escalateDir t dR) oR
        normCo = normal $ Direction (xP centCo) (radCo * tan) (zP centCo)
        denom = (b**2) - (4 * a * c)
        t1 = (-b + sqrt (denom)) / (2 * a)
        t2 = (-b - sqrt (denom)) / (2 * a)

        tan = (radCo / altCo)**2
        a = ((xD dR)**2) + ((zD dR)**2) - (((tan**2) * (yD dR))**2)

        b = (2 * ((xP centCo) - (xP oR)) * (xD dR)) + (2 * ((zP centCo) - (zP oR)) * (zD dR)) + (2 * (tan**2) * (altCo - (yP oR) - (yP centCo)) * (yD dR))
        c = (((xP centCo) - (xP oR))**2) + (((zP centCo) - (zP oR))**2) - (tan**2) * (altCo - (yP oR) - (yP centCo))**2

oneCollision tr@(Triangle (Triangulo {..})) (Ray {..}) =
    case ray1TriangleIntersection oR dR p0Tr p1Tr p2Tr of
        Just (t, intersectionPoint) ->
            let normalVec = (p1Tr #< p0Tr) * (p2Tr #< p0Tr)
                normalVec' = if (dR.*normalVec) > 0 then normal (escalateDir (-1) normalVec) else normal normalVec
            in (Obj t rgbTr dR intersectionPoint normalVec' trTr reflTr idTr tr)
        Nothing -> (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0 tr)


-- |Función auxiliar, devuelve el id interno de las figuras.
{-# INLINE getShapeID #-}
getShapeID :: Shape -> Int
getShapeID (Sphere (Esfera{..})) = idEs
getShapeID (Plane (Plano {..})) = idPl
getShapeID (Triangle (Triangulo {..})) = idTr
getShapeID (Cylinder (Cilindro _ _ _ _ _ _ id)) = id
getShapeID (Rectangle(Rectangulo{..})) = idRe
getShapeID (Acelerator(BVH{..})) = idBvh
getShapeID (Cone (Cono{..})) = idCo
-- getShapeID (Donut (Rosquilla _ _ _ _ _ _ _ id)) = id

-- |Función auxiliar, dado un rayo(descompuesto) y un triángulo(descompuesto) cálcula la intersección.
{-# INLINE ray1TriangleIntersection #-}
ray1TriangleIntersection :: Point3D -> Direction -> Point3D -> Point3D -> Point3D -> Maybe (Float, Point3D)
ray1TriangleIntersection orig dir v1 v2 v3 = do
    let e1 = v2 #< v1
        e2 = v3 #< v1
        h = dir * e2
        a = e1 .* h
    if abs a < 1e-5
        then Nothing
        else do
            let f = 1.0 / a
                s = orig #< v1
                u = f * (s .* h)
            if u < 0.0 || u > 1.0
                then Nothing
                else do
                    let q = s * e1
                        v = f * (dir .* q)
                    if v < 0.0 || u + v > 1.0
                        then Nothing
                        else do
                            let t = f * (e2 .* q)
                            if t > 1e-5
                                then Just (t, movePoint (escalateDir t dir) orig)
                                else Nothing


-- |Función auxiliar, convierte de triangle a triangulo.
{-# INLINE triangleToTriangulo #-}
triangleToTriangulo :: RGB -> (Float,Float,Float) -> Float -> Int -> ([Point3D], TrianglePos,[Point2D], TrianglePos) -> Triangulo
triangleToTriangulo rgb (kd,ke,kr) reflec id (vertices, TrianglePos v1 v2 v3, texturas, TrianglePos t1 t2 t3) =
    (Triangulo
        (vertices !! (v1 - 1)) (vertices !! (v2 - 1)) (vertices !! (v3 - 1)) (texturas !! (t1 - 1)) (texturas !! (t1 - 1)) (texturas !! (t1 - 1))
        rgb (kd,ke,kr) reflec id
    )
    -- where
        -- v1' = vertices !! (v1 - 1)
        -- v2' = vertices !! (v2 - 1)
        -- v3' = vertices !! (v3 - 1)
        -- vNormal = normal $ (v2' #< v1') * (v3' #< v1')

-- |Función básica, convierte los tríangulos y vértices cargados al formato deseado(color,propiedades).
{-# INLINE convertToCustomFormat #-}
convertToCustomFormat :: RGB -> (Float,Float,Float) -> Float -> Int -> ([Point3D], [TrianglePos], [Point2D], [TrianglePos]) -> [Triangulo]
convertToCustomFormat rgb (kd,ke,kr) reflec id (vertices, triangles, texturas, texttring) = map (triangleToTriangulo rgb (kd,ke,kr) reflec id . resolveVertices) $ zip triangles texttring
  where
    resolveVertices ((TrianglePos v1 v2 v3),(TrianglePos t1 t2 t3)) = (vertices, TrianglePos v1 v2 v3, texturas, TrianglePos t1 t2 t3)

-- |Función auxiliar, dada una línea del .obj parsea el triángulo que esta contiene.
{-# INLINE parseTriangle #-}
parseTriangle :: String -> Maybe [TrianglePos]
parseTriangle line = case words line of
    ["f", v1Str, v1Tex, v2Str,v2Tex, v3Str,v3Tex] -> Just $ [TrianglePos (read v1Str) (read v2Str) (read v3Str), TrianglePos (read v1Tex) (read v2Tex) (read v3Tex)]
    ["f", v1Str, v2Str, v3Str] -> Just $ [(TrianglePos (read v1Str) (read v2Str) (read v3Str)),(TrianglePos 0 0 0)]
    _ -> Nothing


-- |Función auxiliar, parsea una línea del .obj a punto 3D.
{-# INLINE parsePoint3D #-}
parsePoint3D :: String -> Maybe Point3D
parsePoint3D line = case words line of
    ["v", xStr, yStr, zStr] -> Just $ Point3D (read xStr) (read yStr) (read zStr)
    _ -> Nothing

-- |Función auxiliar, parsea una línea del .obj a punto 3D.
{-# INLINE parsePoint2D #-}
parsePoint2D :: String -> Maybe Point2D
parsePoint2D line = case words line of
    ["vt", uStr, vStr] -> Just $ Point2D (read uStr) (read vStr)
    _ -> Nothing

-- |Función básica, extrae los vértices y triángulos de un fichero .obj.
{-# INLINE loadObjFile #-}
loadObjFile :: FilePath -> IO ([Point2D],[Point3D], [[TrianglePos]])
loadObjFile filePath = do
    contents <- readFile filePath
    let  lines' = lines contents
         validTexturePoints = mapMaybe parsePoint2D lines'
         validVertices = mapMaybe parsePoint3D lines'
         validTriangles = mapMaybe parseTriangle lines'
    return (validTexturePoints, validVertices, validTriangles)

-- |Función auxiliar, convierte de punto3D a punto3D (convierte contenido a floats).
{-# INLINE vertexToPoint3D #-}
vertexToPoint3D :: Point3D -> Point3D
vertexToPoint3D (Point3D x y z) = Point3D (realToFrac x) (realToFrac y) (realToFrac z)

-- |Función auxiliar, devuelve el mínimo(este debe ser positivo).
{-# INLINE findMinPositive #-}
findMinPositive :: Float -> Float -> Float
findMinPositive x y
    | x > 0 && y > 0 = min x y
    | x > 0          = x
    | y > 0          = y
    | otherwise      = -1
