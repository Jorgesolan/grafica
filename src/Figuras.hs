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
      (.*),
      escalateDir,
      normal, escalatePoint, pointDir, dirPoint, distPoint, addPoints )
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

import qualified Data.DList as DL
import qualified Data.Set as Set

data Camara = Camara Point3D Base
data Esfera = Esfera {centEs :: Point3D, radEs :: Float, rgbEs ::  RGB, trEs :: (Float, Float, Float), reflEs :: Float, idEs :: Int} deriving Show
data Plano = Plano {centPl :: Point3D, normPl :: Direction, rgbPl :: RGB, trPl :: (Float, Float, Float), reflPl :: Float, idPl :: Int} deriving Show
data Triangulo = Triangulo {p0Tr :: Point3D, p1Tr :: Point3D, p2Tr :: Point3D, rgbTr :: RGB, trTr :: (Float, Float, Float), reflTr :: Float, idTr :: Int} deriving Show
data Cilindro = Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int deriving Show
data Rectangulo = Rectangulo {centRe :: Point3D, normRe :: Direction, tngRe :: Direction, altRe :: Float, ancRe :: Float, rgbRe :: RGB, trRe :: (Float, Float, Float), reflRe:: Float, idRe:: Int} deriving Show

data AABB = AABB {p0AB :: Point3D, p1AB ::  Point3D} deriving Show
data BVH = BVH {aabb::AABB, bvhs :: [BVH], triangulos :: [Triangulo], idBvh :: Int} deriving Show
data TrianglePos = TrianglePos { v1 :: Int, v2 :: Int, v3 :: Int } deriving Show

-- data Rosquilla = Rosquilla Point3D Direction Float Float RGB Float Int

data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo | Cylinder Cilindro | Rectangle Rectangulo | Acelerator BVH deriving Show

-- data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo | Donut Rosquilla

data Obj = Obj {mindObj :: Float, rgbObj :: RGB, w0Obj :: Direction, colObj :: Point3D, normObj :: Direction, trObj ::(Float, Float, Float), reflObj :: Float, idObj:: Int} deriving Show

instance Eq Shape where
    a == b = False
instance Ord Shape where
    compare = comparing getShapeID

instance Eq Obj where
    obj == obj1 = mindObj obj == mindObj obj1
instance Ord Obj where
    compare obj obj1 = compare (mindObj obj) (mindObj obj1)

lengthDL :: DL.DList a -> Int
lengthDL = DL.foldr (\_ n -> n + 1) 0

getUV :: Shape -> Point3D -> (Float, Float)
--getUV (Plane (Plano {..})) p = (1,1)
getUV (Sphere (Esfera {..})) p = trace (show (u,v) ) (u,v)
    where
        (Point3D x y z) = p
        (Point3D cx cy cz) = centEs
        u = 0.5 + atan2 (z - cz) (x - cx) / (2 * pi)
        v = 0.5 - asin ((y - cy) / radEs) / pi

getUV (Triangle (Triangulo {..})) p = (1,1)
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


calculateBoundingBox :: [Triangulo] -> AABB
calculateBoundingBox triangles =
    let xs = [x | tri <- triangles, x <- [xP (p0Tr tri), xP (p1Tr tri), xP (p2Tr tri)]]
        ys = [y | tri <- triangles, y <- [yP (p0Tr tri), yP (p1Tr tri), yP (p2Tr tri)]]
        zs = [z | tri <- triangles, z <- [zP (p0Tr tri), zP (p1Tr tri), zP (p2Tr tri)]]
        minPoint = Point3D (minimum xs) (minimum ys) (minimum zs)
        maxPoint = Point3D (maximum xs) (maximum ys) (maximum zs)
    in AABB minPoint maxPoint

splitTriangles :: Int -> [Triangulo] -> [[Triangulo]]
splitTriangles maxSize [] = []
splitTriangles maxSize triangles =
    let (first, rest) = splitAt maxSize triangles
    in first : splitTriangles maxSize rest

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

closestIntersection :: Ray -> [Triangulo] -> (Float, Triangulo)
closestIntersection _ [] = (-1, Triangulo (Point3D 0 0 0) (Point3D 0 0 0) (Point3D 0 0 0) (RGB 0 0 0) (0, 0, 0) 0 0) -- Default value
closestIntersection (Ray {..}) triangles =
    foldr findClosestIntersection (-1, head triangles) triangles
    where
        findClosestIntersection :: Triangulo -> (Float, Triangulo) -> (Float, Triangulo)
        findClosestIntersection triangle (minDist, closestTri) =
            let intersection = rap1TriangleIntersection oR dR (p0Tr triangle) (p1Tr triangle) (p2Tr triangle)
            in case intersection of
                Just (t, _) ->
                    if minDist < 0 || (t > 0 && t < minDist)
                        then (t, triangle)
                        else (minDist, closestTri)
                Nothing -> (minDist, closestTri)



addFig :: Shape -> [Shape] -> [Shape]
addFig (Plane (Plano {..})) shapes = Plane (Plano centPl normPl rgbPl trPl reflPl (length shapes)):shapes
addFig (Sphere (Esfera {..})) shapes = Sphere (Esfera centEs radEs rgbEs trEs reflEs (length shapes)):shapes
addFig (Triangle (Triangulo {..})) shapes = Triangle (Triangulo p0Tr p1Tr p2Tr rgbTr trTr reflTr (length shapes)):shapes
addFig (Cylinder (Cilindro p1 p2 p3 color reflec kr _)) shapes = Cylinder (Cilindro p1 p2 p3 color reflec kr (length shapes)):shapes
addFig (Rectangle(Rectangulo {..})) shapes = Rectangle (Rectangulo centRe normRe tngRe altRe ancRe rgbRe trRe reflRe (length shapes)):shapes
addFig (Acelerator (BVH {..})) shapes = Acelerator (BVH aabb bvhs triangulos (length shapes)):shapes

addFigMult :: [Shape] -> [Shape] -> [Shape]
addFigMult xs shapes = foldl (flip addFig) shapes xs

encenderShapes :: [Shape] -> [Shape]
encenderShapes = map encenderShape


encenderShape :: Shape -> Shape
encenderShape (Plane (Plano {..})) = Plane (Plano centPl normPl rgbPl trPl reflPl (-idPl))
encenderShape (Sphere (Esfera {..})) = Sphere (Esfera centEs radEs rgbEs trEs reflEs (-idEs))
encenderShape (Triangle (Triangulo {..})) = Triangle (Triangulo p0Tr p1Tr p2Tr rgbTr trTr reflTr (-idTr))

{-# INLINE parametricShapeCollision #-}
parametricShapeCollision :: Set.Set Shape -> [Ray] -> [Set.Set Obj]
parametricShapeCollision shapes rays = map (collision shapes) rays
  where
    collision shapes ray = Set.map (`oneCollision` ray) shapes

{-# INLINE oneCollision #-}
oneCollision :: Shape -> Ray -> Obj
oneCollision (Sphere (Esfera {..})) (Ray {..}) =
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
                               then (Obj mind rgbEs dR collisionPoint vectorNormal trEs reflEs idEs)
                               else (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) trEs reflEs 0)) else (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) trEs reflEs 0))

oneCollision (Plane (Plano {..})) (Ray {..}) = (Obj mind rgbPl dR collisionPoint vectorNormal trPl reflPl idPl)
  where
    mind = ((centPl #< oR) .* vectorNormal) / (dR .* vectorNormal)
    collisionPoint = movePoint (escalateDir mind dR) oR
    vectorNormal = if (dR.*normPl) > 0 then normal (escalateDir (-1) normPl) else normal normPl


oneCollision (Cylinder(Cilindro p0 n r color reflec kr id)) (Ray {..}) = (Obj mind color dR collisionPoint vectorNormal reflec kr id)
  where
    mind = findMinPositive t1 t2
    t1 = ((p0 #< oR) .* vectorNormal + sqrt discriminant) / (dR .* vectorNormal)
    t2 = ((p0 #< oR) .* vectorNormal - sqrt discriminant) / (dR .* vectorNormal)
    discriminant = ((oR #< p0) .* (oR #< p0)) - r * r
    collisionPoint = movePoint (escalateDir mind dR) oR
    vectorNormal = if (dR.*n)>0 then normal (escalateDir (-1) n) else normal n


oneCollision (Acelerator (BVH {..})) ray =
    if rayIntersectsAABB ray aabb
        then
            if null bvhs
                then
                    oneCollision (Triangle $ snd(closestIntersection ray triangulos)) ray
                else
                    let childCollisions = filter (\obj -> mindObj obj /= -1) $ map (\ch -> oneCollision (Acelerator ch) ray ) bvhs

                    in case childCollisions of
                        [] -> (Obj (-1) (RGB 0 0 0) (dR ray) (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)
                        _ -> let mindObj = minimum childCollisions
                             in mindObj
        else (Obj (-1) (RGB 0 0 0) (dR ray) (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)


oneCollision  (Rectangle (Rectangulo {..})) (Ray {..})
  | denom /= 0 && t > 0 && withinBounds = (Obj t rgbRe dR (dirPoint collisionPoint) normRe' trRe reflRe idRe)
  | otherwise = (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) trRe reflRe 0)
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


oneCollision (Triangle (Triangulo {..})) (Ray {..}) =
    case rap1TriangleIntersection oR dR p0Tr p1Tr p2Tr of
        Just (t, intersectionPoint) ->
            let normalVec = (p1Tr #< p0Tr) * (p2Tr #< p0Tr)
                normalVec' = if (dR.*normalVec) > 0 then normal (escalateDir (-1) normalVec) else normal normalVec
            in (Obj t rgbTr dR intersectionPoint normalVec' trTr reflTr idTr)
        Nothing -> (Obj (-1) (RGB 0 0 0) dR (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)



getShapeID :: Shape -> Int
getShapeID (Sphere (Esfera{..})) = idEs
getShapeID (Plane (Plano {..})) = idPl
getShapeID (Triangle (Triangulo {..})) = idTr
getShapeID (Cylinder (Cilindro _ _ _ _ _ _ id)) = id
getShapeID (Rectangle(Rectangulo{..})) = idRe
getShapeID (Acelerator(BVH{..})) = idBvh
-- getShapeID (Donut (Rosquilla _ _ _ _ _ _ _ id)) = id

rap1TriangleIntersection :: Point3D -> Direction -> Point3D -> Point3D -> Point3D -> Maybe (Float, Point3D)
rap1TriangleIntersection orig dir v1 v2 v3 = do
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


-- Convert Triangle to Triangulo
triangleToTriangulo :: RGB -> (Float,Float,Float) -> Float -> Int -> ([Point3D], TrianglePos) -> Triangulo
triangleToTriangulo rgb (kd,ke,kr) reflec id (vertices, TrianglePos v1 v2 v3) =
    (Triangulo
        (vertices !! (v1 - 1)) (vertices !! (v2 - 1)) (vertices !! (v3 - 1))
        rgb (kd,ke,kr) reflec id
    )
    where
        v1' = vertices !! (v1 - 1)
        v2' = vertices !! (v2 - 1)
        v3' = vertices !! (v3 - 1)
        vNormal = normal $ (v2' #< v1') * (v3' #< v1')

-- Convert loaded vertices and triangles to custom format
convertToCustomFormat :: RGB -> (Float,Float,Float) -> Float -> ([Point3D], [TrianglePos]) -> [Triangulo]
convertToCustomFormat rgb (kd,ke,kr) reflec (vertices, triangles) = map (triangleToTriangulo rgb (kd,ke,kr) reflec 0 .resolveVertices) triangles
  where
    resolveVertices (TrianglePos v1 v2 v3) = (vertices, TrianglePos v1 v2 v3)

-- Parse a line of the .obj file into a Triangle
{-# INLINE parseTriangle #-}
parseTriangle :: String -> Maybe TrianglePos
parseTriangle line = case words line of
    ["f", v1Str, v2Str, v3Str] -> Just $ TrianglePos (read v1Str) (read v2Str) (read v3Str)
    _ -> Nothing

-- Parse a line of the .obj file into a Point3D
{-# INLINE parsePoint3D #-}
parsePoint3D :: String -> Maybe Point3D
parsePoint3D line = case words line of
    ["v", xStr, yStr, zStr] -> Just $ Point3D (read xStr) (read yStr) (read zStr)
    _ -> Nothing


-- Load the vertices and triangles from the .obj file
loadObjFile :: FilePath -> IO ([Point3D], [TrianglePos])
loadObjFile filePath = do
    contents <- readFile filePath
    let  lines' = lines contents
         (vertices, triangles) = foldr splitLines ([], []) lines'
         validVertices = mapMaybe parsePoint3D lines'
         validTriangles = mapMaybe parseTriangle lines'
    return (validVertices, validTriangles)
  where
    splitLines line (vertices, triangles)
        | null (words line) = (vertices, triangles)
        | Just vertex <- parsePoint3D line = (vertex : vertices, triangles)
        | Just triangle <- parseTriangle line = (vertices, triangle : triangles)
        | otherwise = (vertices, triangles)

-- Convert Point3D to Point3D with Float values
{-# INLINE vertexToPoint3D #-}
vertexToPoint3D :: Point3D -> Point3D
vertexToPoint3D (Point3D x y z) = Point3D (realToFrac x) (realToFrac y) (realToFrac z)

findMinPositive :: Float -> Float -> Float
findMinPositive x y
    | x > 0 && y > 0 = min x y
    | x > 0          = x
    | y > 0          = y
    | otherwise      = -1
