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
      normal, escalatePoint, pointDir, dirPoint, getX, getY, getZ, distPoint, addPoints )
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

data Camara = Camara Point3D Base
data Esfera = Esfera {centEs :: Point3D, radEs :: Float, rgbEs ::  RGB, trEs :: (Float, Float, Float), reflEs :: Float, idEs :: Int}
data Plano = Plano {centPl :: Point3D, normPl :: Direction, rgbPl :: RGB, trPl :: (Float, Float, Float), reflPl :: Float, idPl :: Int}
data Triangulo = Triangulo {xTr :: Point3D, yTr :: Point3D, zTr :: Point3D, rgbTr :: RGB, trTr :: (Float, Float, Float), reflTr :: Float, idTr :: Int}
data Cilindro = Cilindro Point3D Direction Float RGB (Float, Float, Float) Float Int
data Rectangulo = Rectangulo {centRe :: Point3D, normRe :: Direction, tngRe :: Direction, altRe :: Float, ancRe :: Float, rgbRe :: RGB, trRe :: (Float, Float, Float), reflRe:: Float, idRe:: Int}

data AABB = AABB Point3D Point3D
data BVH = BVH {aabb::AABB, bvhs :: [BVH], triangulos :: [Triangulo], idBvh :: Int}

-- data Rosquilla = Rosquilla Point3D Direction Float Float RGB Float Int

data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo | Cylinder Cilindro | Rectangle Rectangulo | Acelerator BVH
-- data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo | Donut Rosquilla

data Obj = Obj {rgbObj :: RGB, w0Obj :: Direction, colObj :: Point3D, normObj :: Direction, trObj ::(Float, Float, Float), reflObj :: Float, idObj:: Int} deriving Show


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
    let xs = [x | tri <- triangles, x <- [getX (xTr tri), getX (yTr tri), getX (zTr tri)]]
        ys = [y | tri <- triangles, y <- [getY (xTr tri), getY (yTr tri), getY (zTr tri)]]
        zs = [z | tri <- triangles, z <- [getZ (xTr tri), getZ (yTr tri), getZ (zTr tri)]]
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
rayIntersectsAABB (Ray (Point3D ox oy oz) (Direction dx dy dz)) (AABB (Point3D minx miny minz) (Point3D maxx maxy maxz)) =
    let tx1 = (minx - ox) / dx
        tx2 = (maxx - ox) / dx
        ty1 = (miny - oy) / dy
        ty2 = (maxy - oy) / dy
        tz1 = (minz - oz) / dz
        tz2 = (maxz - oz) / dz
        tmin = maximum [min tx1 tx2, min ty1 ty2, min tz1 tz2]
        tmax = minimum [max tx1 tx2, max ty1 ty2, max tz1 tz2]
    in tmax >= tmin && tmax >= 0

closestIntersection :: Ray -> [Triangulo] -> (Float, Triangulo)
closestIntersection _ [] = (-1, Triangulo (Point3D 0 0 0) (Point3D 0 0 0) (Point3D 0 0 0) (RGB 0 0 0) (0, 0, 0) 0 0) -- Default value
closestIntersection (Ray rayOrigin rayDir) triangles =
    foldr findClosestIntersection (-1, head triangles) triangles
    where
        findClosestIntersection :: Triangulo -> (Float, Triangulo) -> (Float, Triangulo)
        findClosestIntersection triangle@(Triangulo p1 p2 p3 _ _ _ _) (minDist, closestTri) =
            let intersection = rayTriangleIntersection rayOrigin rayDir p1 p2 p3
            in case intersection of
                Just (t, _) ->
                    if minDist < 0 || (t > 0 && t < minDist)
                        then (t, triangle)
                        else (minDist, closestTri)
                Nothing -> (minDist, closestTri)



addFig :: Shape -> [Shape] -> [Shape]
addFig (Plane (Plano {..})) shapes = Plane (Plano centPl normPl rgbPl trPl reflPl (length shapes)):shapes
addFig (Sphere (Esfera {..})) shapes = Sphere (Esfera centEs radEs rgbEs trEs reflEs (length shapes)):shapes
addFig (Triangle (Triangulo {..})) shapes = Triangle (Triangulo xTr yTr zTr rgbTr trTr reflTr (length shapes)):shapes
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
encenderShape (Triangle (Triangulo {..})) = Triangle (Triangulo xTr yTr zTr rgbTr trTr reflTr (-idTr))

{-# INLINE parametricShapeCollision #-}
parametricShapeCollision :: [Shape] -> [Ray] -> [[(Float, Obj)]]
parametricShapeCollision shapes rays = map (collision rays) shapes
  where
    collision rays shape = map (`oneCollision` shape) rays

oneCollision :: Ray -> Shape -> (Float, Obj)
oneCollision (Ray p1 d) (Sphere (Esfera {..})) =
    let f = p1 #< centEs
        a = d .* d
        b = 2.0 * (f .* d)
        c = f .* f - radEs * radEs
        raiz = b * b - 4.0*a*c
    in
        (if raiz > 0 then (let t0 = (-b + sqrt raiz) / (2.0 * a)
                               t1 = (-b - sqrt raiz) / (2.0 * a)
                               mind = findMinPositive t0 t1
                               collisionPoint = movePoint (escalateDir mind d) p1
                               vectorNormal = normal $ collisionPoint #< centEs
                           in if t0 > 0 || t1 > 0
                               then (mind, Obj rgbEs d collisionPoint vectorNormal trEs reflEs idEs)
                               else (-1, Obj (RGB 0 0 0) d (Point3D 0 0 0) (Direction 0 0 0) trEs reflEs 0)) else (-1, Obj (RGB 0 0 0) d (Point3D 0 0 0) (Direction 0 0 0) trEs reflEs 0))

oneCollision (Ray p1 d) (Plane (Plano {..})) = (mind, Obj rgbPl d collisionPoint vectorNormal trPl reflPl idPl)
  where
    mind = ((centPl #< p1) .* vectorNormal) / (d .* vectorNormal)
    collisionPoint = movePoint (escalateDir mind d) p1
    vectorNormal = if (d.*normPl) > 0 then normal (escalateDir (-1) normPl) else normal normPl


oneCollision (Ray p1 d) (Cylinder(Cilindro p0 n r color reflec kr id)) = (mind, Obj color d collisionPoint vectorNormal reflec kr id)
  where
    mind = findMinPositive t1 t2
    t1 = ((p0 #< p1) .* vectorNormal + sqrt discriminant) / (d .* vectorNormal)
    t2 = ((p0 #< p1) .* vectorNormal - sqrt discriminant) / (d .* vectorNormal)
    discriminant = ((p1 #< p0) .* (p1 #< p0)) - r * r
    collisionPoint = movePoint (escalateDir mind d) p1
    vectorNormal = if (d.*n)>0 then normal (escalateDir (-1) n) else normal n


oneCollision ray@(Ray rayOrigin rayDir) (Acelerator (BVH bbox children triangles _)) =
    if rayIntersectsAABB ray bbox
        then
            if null children
                then
                    oneCollision ray (Triangle $ snd(closestIntersection ray triangles))
                else
                    let childCollisions = filter (\(t, _) -> t /= -1) $ map (oneCollision ray . Acelerator) children

                    in case childCollisions of
                        [] -> (-1, Obj (RGB 0 0 0) rayDir (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)
                        _ -> let (minT, minObj) = minimumBy (comparing fst) childCollisions
                             in (minT, minObj)
        else (-1, Obj (RGB 0 0 0) rayDir (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)

-- oneCollision ray@(Ray rayOrigin rayDir) (Acelerator (BVH bbox children triangles _)) =
--     if rayIntersectsAABB ray bbox
--         then
--             if null children
--                 then
--                     let (t, obj) = oneCollision ray (Triangle $ snd(closestIntersection ray triangles))
--                     in trace ("Leaf node hit. Closest intersection: " ++ show t ++ ", Object: " ++ show triangles) (t, obj)
--                 else
--                     let (t1, obj1) = oneCollision ray (Acelerator $ head children)
--                         (t2, obj2) = oneCollision ray (Acelerator $ children !! 1)
--                         result = if t1 < t2 then (t1, obj1) else (t2, obj2)
--                     in trace ("Intermediate node hit. T1: " ++ show t1 ++ ", Obj1: " ++ show obj1 ++ ", T2: " ++ show t2 ++ ", Obj2: " ++ show obj2) result
--         else trace "No intersection with AABB." (-1, Obj (RGB 0 0 0) rayDir (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)

oneCollision (Ray p d) (Rectangle (Rectangulo {..}))
  | denom /= 0 && t > 0 && withinBounds = (t, Obj rgbRe d (dirPoint collisionPoint) normRe' trRe reflRe idRe)
  | otherwise = (-1, Obj (RGB 0 0 0) d (Point3D 0 0 0) (Direction 0 0 0) trRe reflRe 0)
  where
    offset = collisionPoint - pointDir centRe
    localX = offset .* right
    localY = offset .* up
    halfWidth = ancRe / 2
    halfHeight = altRe / 2
    collisionPoint = pointDir p + escalateDir t d
    t = (centRe #< p) .* normRe / denom
    withinBounds = -halfWidth <= localX && localX <= halfWidth && -halfHeight <= localY && localY <= halfHeight
    denom = d .* normRe
    right = normal tngRe
    up = normRe * right
    normRe' = if d .* normRe > 0 then normal (escalateDir (-1) normRe) else normal normRe


oneCollision (Ray rayOrigin rayDir) (Triangle (Triangulo {..})) =
    case rayTriangleIntersection rayOrigin rayDir xTr yTr zTr of
        Just (t, intersectionPoint) ->
            let normalVec = (yTr #< xTr) * (zTr #< xTr)
                normalVec' = if (rayDir.*normalVec) > 0 then normal (escalateDir (-1) normalVec) else normal normalVec
            in (t, Obj rgbTr rayDir intersectionPoint normalVec' trTr reflTr idTr)
        Nothing -> (-1, Obj (RGB 0 0 0) rayDir (Point3D 0 0 0) (Direction 0 0 0) (0,0,0) 0 0)

getShapeID :: Shape -> Int
getShapeID (Sphere (Esfera{..})) = idEs
getShapeID (Plane (Plano _ _ _ _  _ id)) = id
getShapeID (Triangle (Triangulo _ _ _ _ _ _ id)) = id
getShapeID (Cylinder (Cilindro _ _ _ _ _ _ id)) = id
getShapeID (Rectangle(Rectangulo{..})) = idRe
getShapeID (Acelerator(BVH{..})) = idBvh
-- getShapeID (Donut (Rosquilla _ _ _ _ _ _ _ id)) = id

rayTriangleIntersection :: Point3D -> Direction -> Point3D -> Point3D -> Point3D -> Maybe (Float, Point3D)
rayTriangleIntersection orig dir v1 v2 v3 = do
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

data TrianglePos = TrianglePos { v1 :: Int, v2 :: Int, v3 :: Int } deriving Show



-- Parse a line of the .obj file into a Point3D
{-# INLINE parsePoint3D #-}
parsePoint3D :: String -> Maybe Point3D
parsePoint3D line = case words line of
    ["v", xStr, yStr, zStr] -> Just $ Point3D (read xStr) (read yStr) (read zStr)
    _ -> Nothing

-- Parse a line of the .obj file into a Triangle
{-# INLINE parseTriangle #-}
parseTriangle :: String -> Maybe TrianglePos
parseTriangle line = case words line of
    ["f", v1Str, v2Str, v3Str] -> Just $ TrianglePos (read v1Str) (read v2Str) (read v3Str)
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

-- Convert Triangle to Triangulo
-- triangleToTriangulo :: ([Point3D], TrianglePos) -> Shape
-- triangleToTriangulo (vertices, TrianglePos v1 v2 v3) =
--     Triangle (Triangulo
--         (vertices !! (v1 - 1)) (vertices !! (v2 - 1)) (vertices !! (v3 - 1))
--         (RGB 255 255 255) (0.8,0,0) 0 0
--     )
--     where
--         v1' = vertices !! (v1 - 1)
--         v2' = vertices !! (v2 - 1)
--         v3' = vertices !! (v3 - 1)
--         vNormal = normal $ (v2' #< v1') * (v3' #< v1')
-- Convert loaded vertices and triangles to custom format
-- convertToCustomFormat :: ([Point3D], [TrianglePos]) -> [Shape]
-- convertToCustomFormat (vertices, triangles) = map (triangleToTriangulo.resolveVertices) triangles
--   where
--     resolveVertices (TrianglePos v1 v2 v3) = (vertices, TrianglePos v1 v2 v3)
-- Convert Triangle to Triangulo
triangleToTriangulo :: ([Point3D], TrianglePos) -> Triangulo
triangleToTriangulo (vertices, TrianglePos v1 v2 v3) =
    (Triangulo
        (vertices !! (v1 - 1)) (vertices !! (v2 - 1)) (vertices !! (v3 - 1))
        (RGB 200 200 200) (1,0,0) 0 0
    )
    where
        v1' = vertices !! (v1 - 1)
        v2' = vertices !! (v2 - 1)
        v3' = vertices !! (v3 - 1)
        vNormal = normal $ (v2' #< v1') * (v3' #< v1')
-- Convert loaded vertices and triangles to custom format
convertToCustomFormat :: ([Point3D], [TrianglePos]) -> [Triangulo]
convertToCustomFormat (vertices, triangles) = map (triangleToTriangulo.resolveVertices) triangles
  where
    resolveVertices (TrianglePos v1 v2 v3) = (vertices, TrianglePos v1 v2 v3)


findMinPositive :: Float -> Float -> Float
findMinPositive x y
    | x > 0 && y > 0 = min x y
    | x > 0          = x
    | y > 0          = y
    | otherwise      = -1
