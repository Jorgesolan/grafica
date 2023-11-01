{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Figuras where
import Elem3D
import Debug.Trace

import Data.Maybe (mapMaybe)
import Control.Monad (guard)

data Camara = Camara !Point3D !Base
data Esfera = Esfera !Point3D !Float !RGB !Float !Float !Int
data Plano = Plano !Point3D !Direction !RGB !Float !Float !Int
data Triangulo = Triangulo !Point3D !Point3D !Point3D !RGB !Float !Float !Int
data Rosquilla = Rosquilla !Point3D !Direction !Float  !Float !RGB !Float !Float !Int

data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo | Donut Rosquilla

type Obj = (RGB, Float, Point3D, Direction, Float, Int)

{-# INLINE parametricShapeCollision #-}
parametricShapeCollision :: [Shape] -> [Ray] -> [[(Float, Obj)]]
parametricShapeCollision shapes rays = map (collision rays) shapes
  where
    collision rays shape = map (oneCollision shape) rays

oneCollision :: Shape -> Ray -> (Float, Obj)
oneCollision (Sphere (Esfera p0 r color reflec lum id)) (Ray p1 d m) = 
    let f = p1 #< p0
        a = d .* d
        b = 2.0 * (f .* d)
        c = f .* f - r * r
        raiz = b * b - 4*a*c
        findMinPositive :: Float -> Float -> Float
        findMinPositive x y
            | x > 0 && y > 0 = min x y
            | x > 0          = x
            | y > 0          = y
            | otherwise      = -1      
    in
        if  | raiz > 0 ->
                let !t0 = (-b + sqrt raiz) / (2.0 * a)
                    !t1 = (-b - sqrt raiz) / (2.0 * a)
                    !mind = findMinPositive t0 t1
                    collisionPoint = movePoint (escalateDir mind d) p1
                    vectorNormal = normal $ collisionPoint #< p0
                in if t0 > 0 || t1 > 0
                    then (mind, (color, reflec, collisionPoint, vectorNormal, lum, id))
                    else ((-1), (color, reflec, Point3D (-1) (-1) (-1), Direction (-1) (-1) (-1), lum, id))
            | otherwise -> ((-1), (color, reflec, Point3D (-1) (-1) (-1), Direction (-1) (-1) (-1), lum, id))  

oneCollision (Plane (Plano p0 n color reflec lum id)) (Ray p1 d m) = (mind, (color, reflec, collisionPoint, vectorNormal, lum, id))
  where
    mind = ((p0 #< p1) .* vectorNormal) / (d .* vectorNormal)
    collisionPoint = movePoint (escalateDir mind d) p1
    vectorNormal = normal n

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


oneCollision (Donut (Rosquilla p0 d r1 r2 color reflec lum id)) (Ray p1 d1 m) = 
    let f = p1 #< p0
        a = d1 .* d1
        b = 2.0 * (f .* d1)
        c = f .* f - r2 * r2 - r1 * r1
        raiz = b * b - 4*a*c
        findMinPositive :: Float -> Float -> Float
        findMinPositive x y
            | x > 0 && y > 0 = min x y
            | x > 0          = x
            | y > 0          = y
            | otherwise      = -1      
    in
        if  | raiz > 0 ->
                let !t0 = (-b + sqrt raiz) / (2.0 * a)
                    !t1 = (-b - sqrt raiz) / (2.0 * a)
                    mind = findMinPositive t0 t1
                    collisionPoint = movePoint (escalateDir mind d1) p1
                    vectorNormal = normal $ collisionPoint #< p0
                in if t0 > 0 || t1 > 0
                    then (mind, (color, reflec, collisionPoint, vectorNormal, lum, id))
                    else ((-1), (color, reflec, Point3D (-1) (-1) (-1), Direction (-1) (-1) (-1), lum, id))
            | otherwise -> ((-1), (color, reflec, Point3D (-1) (-1) (-1), Direction (-1) (-1) (-1), lum, id)) 



oneCollision (Triangle (Triangulo p1 p2 p3 color reflec lum id)) (Ray rayOrigin rayDir m) =
    case rayTriangleIntersection rayOrigin rayDir p1 p2 p3 of
        Just (t, intersectionPoint) ->
            let normalVec = normal $ (p2 #< p1) * (p3 #< p1)
            in (t, (color, reflec, intersectionPoint, normalVec, lum, id))
        Nothing -> ((-1), (color, 0.0, Point3D (-1) (-1) (-1), Direction (-1) (-1) (-1), 0, 0))

getShapeID :: Shape -> Int
getShapeID (Sphere (Esfera _ _ _ _ _ id)) = id

getShapeID (Plane (Plano _ _ _ _ _ id)) = id

getShapeID (Triangle (Triangulo _ _ _ _ _ _ id)) = id
getShapeID (Donut (Rosquilla _ _ _ _ _ _ _ id)) = id
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
    let lines' = lines contents
    let (vertices, triangles) = foldr splitLines ([], []) lines'
    let validVertices = mapMaybe parsePoint3D lines'
    let validTriangles = mapMaybe parseTriangle lines'
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
        (RGB 0 0 255) 0 0 0
    )
-- Convert loaded vertices and triangles to custom format
convertToCustomFormat :: ([Point3D], [TrianglePos]) -> [Shape]
convertToCustomFormat (vertices, triangles) = map (triangleToTriangulo.resolveVertices) triangles
  where
    resolveVertices (TrianglePos v1 v2 v3) = (vertices, TrianglePos v1 v2 v3)



-- -- Helper function to solve a quartic equation and return its roots as a list
-- solveQuartic :: Float -> Float -> Float -> Float -> Float -> [Float]
-- solveQuartic a4 a3 a2 a1 a0
--     | a4 == 0 = solveCubic a3 a2 a1 a0
--     | otherwise =
--         let a3' = a3 / a4
--             a2' = a2 / a4
--             a1' = a1 / a4
--             a0' = a0 / a4
--             a2Squared = a2' * a2'
--             a1Cubed = a1' * a1' * a1'
--             q = (3.0 * a1' - a2Squared) / 9.0
--             r = (9.0 * a1' * a2' - 27.0 * a0' - 2.0 * a2Squared * a2Squared) / 54.0
--             discriminant = q * q * q + r * r
--         in
--             if discriminant >= 0
--             then
--                 let s = cbrt (r + sqrt discriminant)
--                     t = cbrt (r - sqrt discriminant)
--                     root1 = -a2' / 3.0 - (s + t) - q / (3.0 * (s * t))
--                     root2 = -a2' / 3.0 + 0.5 * (s + t) - q / (3.0 * s * t)
--                     root3 = -a2' / 3.0 + 0.5 * (s + t) - q / (3.0 * s * t)
--                 in [root1, root2, root3]
--             else
--                 let theta = acos (r / sqrt (-q * q * q))
--                     sqrtQ = sqrt (-q)
--                     twoSqrtQ = 2.0 * sqrtQ
--                     root1 = twoSqrtQ * cos (theta / 3.0) - a2' / 3.0
--                     root2 = twoSqrtQ * cos ((theta + 2 * pi) / 3.0) - a2' / 3.0
--                     root3 = twoSqrtQ * cos ((theta + 4 * pi) / 3.0) - a2' / 3.0
--                 in [root1, root2, root3]

-- -- Helper function to find the smallest positive root from a list of roots
-- findSmallestPositiveRoot :: [Float] -> Float
-- findSmallestPositiveRoot roots = minimum [root | root <- roots, root > 0.0]
-- findSmallestPositiveRoot [] = -1

-- -- Helper function to calculate the cube root
-- cbrt :: Float -> Float
-- cbrt x = if x < 0.0 then -((-x) ** (1.0 / 3.0)) else x ** (1.0 / 3.0)

-- -- Helper function to solve a cubic equation and return its roots as a list
-- solveCubic :: Float -> Float -> Float -> Float -> [Float]
-- solveCubic a3 a2 a1 a0
--     | a3 == 0 = solveQuadratic a2 a1 a0
--     | otherwise =
--         let a2' = a2 / a3
--             a1' = a1 / a3
--             a0' = a0 / a3
--             q = (3.0 * a1' - a2' * a2') / 9.0
--             r = (9.0 * a1' * a2' - 27.0 * a0' - 2.0 * a2' * a2' * a2') / 54.0
--             discriminant = q * q * q + r * r
--         in
--             if discriminant >= 0
--             then
--                 let s = cbrt (r + sqrt discriminant)
--                     t = cbrt (r - sqrt discriminant)
--                     root1 = -a2 / 3.0 - (s + t) + q / (3.0 * s * t)
--                     root2 = -a2 / 3.0 + 0.5 * (s + t) - q / (3.0 * s * t)
--                     root3 = -a2 / 3.0 + 0.5 * (s + t) - q / (3.0 * s * t)
--                 in [root1, root2, root3]
--             else
--                 let theta = acos (r / sqrt (-q * q * q))
--                     sqrtQ = sqrt (-q)
--                     twoSqrtQ = 2.0 * sqrtQ
--                     root1 = twoSqrtQ * cos (theta / 3.0) - a2 / 3.0
--                     root2 = twoSqrtQ * cos ((theta + 2 * pi) / 3.0) - a2 / 3.0
--                     root3 = twoSqrtQ * cos ((theta + 4 * pi) / 3.0) - a2 / 3.0
--                 in [root1, root2, root3]

-- -- Helper function to solve a quadratic equation and return its roots as a list
-- solveQuadratic :: Float -> Float -> Float -> [Float]
-- solveQuadratic a2 a1 a0
--     | a2 == 0 = if a1 == 0 then [] else [-a0 / a1]
--     | otherwise =
--         let discriminant = a1 * a1 - 4 * a2 * a0
--         in
--             if discriminant < 0
--             then []
--             else
--                 let sqrtDiscriminant = sqrt discriminant
--                     root1 = (-a1 + sqrtDiscriminant) / (2 * a2)
--                     root2 = (-a1 - sqrtDiscriminant) / (2 * a2)
--                 in [root1, root2]