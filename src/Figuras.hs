{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Figuras where
import Elem3D
import Debug.Trace (trace)

import Data.Maybe (mapMaybe)


data Camara = Camara Point3D Base
data Esfera = Esfera Point3D Float RGB (Float, Float, Float) Int
data Plano = Plano Point3D Direction RGB (Float, Float, Float) Int
data Triangulo = Triangulo Point3D Point3D Point3D RGB (Float, Float, Float) Int
-- data Rosquilla = Rosquilla Point3D Direction Float Float RGB Float Int

data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo 
-- data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo | Donut Rosquilla

data Obj = Obj RGB Direction Point3D Direction (Float, Float, Float) Int deriving Show

{-# INLINE obtenerPunto #-}
obtenerPunto :: Obj -> Point3D
obtenerPunto (Obj _ _ point _ _ _) = point

addFig :: Shape -> [Shape] -> [Shape]
addFig (Plane (Plano p0 n color reflec _)) shapes = (Plane (Plano p0 n color reflec (length shapes))):shapes
addFig (Sphere (Esfera p0 r color reflec _)) shapes = (Sphere (Esfera p0 r color reflec (length shapes))):shapes
addFig (Triangle (Triangulo p1 p2 p3 color reflec _)) shapes = (Triangle (Triangulo p1 p2 p3 color reflec (length shapes))):shapes

addFigMult :: [Shape] -> [Shape] -> [Shape]
addFigMult [] shapes = shapes
addFigMult (x:xs) shapes = addFigMult xs $ addFig x shapes

{-# INLINE parametricShapeCollision #-}
parametricShapeCollision :: [Shape] -> [Ray] -> [[(Float, Obj)]]
parametricShapeCollision shapes rays = map (collision rays) shapes
  where
    collision rays shape = map (\(ray)-> oneCollision ray shape) rays

oneCollision :: Ray -> Shape -> (Float, Obj)
oneCollision (Ray p1 d) (Sphere (Esfera p0 r color reflec id)) = 
    let f = p1 #< p0
        a = d .* d
        b = 2.0 * (f .* d)
        c = f .* f - r * r
        raiz = b * b - 4.0*a*c
        findMinPositive :: Float -> Float -> Float
        findMinPositive x y
            | x > 0 && y > 0 = min x y
            | x > 0          = x
            | y > 0          = y
            | otherwise      = -1      
    in
        if  | raiz > 0 ->
                let t0 = (-b + sqrt raiz) / (2.0 * a)
                    t1 = (-b - sqrt raiz) / (2.0 * a)
                    mind = findMinPositive t0 t1
                    collisionPoint = movePoint (escalateDir mind d) p1
                    vectorNormal = normal $ collisionPoint #< p0
                in if t0 > 0 || t1 > 0
                    then (mind, (Obj color d collisionPoint vectorNormal reflec id))
                    else ((-1), (Obj color d (Point3D (0) (0) (0)) (Direction (0) (0) (0)) reflec id))
            | otherwise -> ((-1), (Obj color d (Point3D (0) (0) (0)) (Direction (0) (0) (0)) reflec id)) 

oneCollision (Ray p1 d) (Plane (Plano p0 n color reflec id)) = (mind, (Obj color d collisionPoint vectorNormal reflec id))
  where
    mind = ((p0 #< p1) .* vectorNormal) / (d .* vectorNormal)
    collisionPoint = movePoint (escalateDir mind d) p1
    vectorNormal = if (d.*n)>0 then normal (escalateDir (-1) n) else normal n

-- oneCollision (Donut (Rosquilla p0 d r1 r2 color reflec lum id)) (Ray p1 d1 m) = 
--     let dp = p1 #< p0
--         a = d1 .* d1
--         b = 2.0 * (dp .* d1)
--         c = (dp .* dp) - r1 * r1 - r2 * r2
--         discriminant = b * b - 4 * a * c
--     in
--     if discriminant > 0 then
--         let t0 = (-b + sqrt discriminant) / (2.0 * a)
--             t1 = (-b - sqrt discriminant) / (2.0 * a)
--             t = min t0 t1 -- Choose the smaller positive root
--             collisionPoint = movePoint (escalateDir t d1) p1
--             torusNormal = normalize $ dp #- (dp .* d1) .* d1 -- Calculate torus normal
--         in
--         if t > 0 then
--             (t, (color, reflec, collisionPoint, torusNormal, lum, id))
--         else
--             (-1, (color, reflec, Point3D (-1) (-1) (-1), Direction (-1) (-1) (-1), lum, id))
--     else
--         (-1, (color, reflec, Point3D (-1) (-1) (-1), Direction (-1) (-1) (-1), lum, id))


-- oneCollision (Donut (Rosquilla p0 d r1 r2 color reflec id)) (Ray p1 d1) = 
--     let f = p1 #< p0
--         a = d1 .* d1
--         b = 2.0 * (f .* d1)
--         c = f .* f - r2 * r2 - r1 * r1
--         raiz = b * b - 4*a*c
--         findMinPositive :: Float -> Float -> Float
--         findMinPositive x y
--             | x > 0 && y > 0 = min x y
--             | x > 0          = x
--             | y > 0          = y
--             | otherwise      = -1      
--     in
--         if  | raiz > 0 ->
--                 let t0 = (-b + sqrt raiz) / (2.0 * a)
--                     t1 = (-b - sqrt raiz) / (2.0 * a)
--                     mind = findMinPositive t0 t1
--                     collisionPoint = movePoint (escalateDir mind d1) p1
--                     vectorNormal = normal $ collisionPoint #< p0
--                 in if t0 > 0 || t1 > 0
--                     then (mind, (Obj color collisionPoint vectorNormal reflec id))
--                     else ((-1), (Obj color (Point3D (-1) (-1) (-1)) (Direction (-1) (-1) (-1)) reflec id))
--             | otherwise -> ((-1), (Obj color (Point3D (-1) (-1) (-1)) (Direction (-1) (-1) (-1)) reflec id))



oneCollision (Ray rayOrigin rayDir) (Triangle (Triangulo p1 p2 p3 color reflec id)) =
    case rayTriangleIntersection rayOrigin rayDir p1 p2 p3 of
        Just (t, intersectionPoint) ->
            let normalVec = normal $ (p2 #< p1) * (p3 #< p1)
            in (t, (Obj color rayDir intersectionPoint normalVec reflec id))
        Nothing -> ((-1), (Obj color rayDir (Point3D (0) (0) (0)) (Direction (0) (0) (0)) reflec id))

getShapeID :: Shape -> Int
getShapeID (Sphere (Esfera _ _ _ _  id)) = id
getShapeID (Plane (Plano _ _ _ _  id)) = id
getShapeID (Triangle (Triangulo _ _ _ _ _ id)) = id
-- getShapeID (Donut (Rosquilla _ _ _ _ _ _ _ id)) = id

rayTriangleIntersection :: Point3D -> Direction -> Point3D -> Point3D -> Point3D -> Maybe (Float, Point3D)
rayTriangleIntersection orig dir v1 v2 v3 = do
    let e1 = v2 #< v1
        e2 = v3 #< v1
        h = dir * e2
        a = e1 .* h
    if abs a < 1e-6
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
                            if t > 1e-6
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
triangleToTriangulo :: ([Point3D], TrianglePos) -> Shape
triangleToTriangulo (vertices, TrianglePos v1 v2 v3) =
    Triangle(Triangulo
        (vertices !! (v1 - 1)) (vertices !! (v2 - 1)) (vertices !! (v3 - 1))
        (RGB 0 0 255) (0,0,0) 0
    )
-- Convert loaded vertices and triangles to custom format
convertToCustomFormat :: ([Point3D], [TrianglePos]) -> [Shape]
convertToCustomFormat (vertices, triangles) = map (triangleToTriangulo.resolveVertices) triangles
  where
    resolveVertices (TrianglePos v1 v2 v3) = (vertices, TrianglePos v1 v2 v3)
