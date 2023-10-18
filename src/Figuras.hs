module Figuras where
import Elem3D
import Debug.Trace
import Text.Printf (printf)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
data Camara = Camara Point3D Base
data Esfera = Esfera Point3D Float RGB Float Int
data Plano = Plano Point3D Direction RGB Float Int
data Triangulo = Triangulo Point3D Point3D Point3D RGB Float Int
data Shape = Sphere Esfera | Plane Plano | Triangle Triangulo

parametricShapeCollision :: [Shape] -> [Ray] -> [[(Float, (RGB, Float, Point3D, Direction,Int))]]
parametricShapeCollision shapes rays = map (collision rays) shapes
  where
    collision rays shape = map (oneCollision shape) rays

oneCollision :: Shape -> Ray -> (Float, (RGB, Float, Point3D, Direction, Int))
oneCollision (Sphere (Esfera p0 r color reflec id)) (Ray p1 d m)
  | raiz >= 0  = (mind, (color, reflec, collisionPoint, vectorNormal, id))
  | otherwise  = (1/0, (color, reflec, Point3D (1/0) (1/0) (1/0), Direction (1/0) (1/0) (1/0), id))
  where
    f = p1 #< p0
    a = d .* d
    b = 2 * (f .* d)
    c = f .* f - r ** 2
    raiz = b ** 2 - 4 * a * c
    resul = sqrt raiz
    t0 = (-b + resul) / (2 * a)
    t1 = (-b - resul) / (2 * a)

    findMinPositive :: Float -> Float -> Float
    findMinPositive x y
      | x > 0 && y > 0 = min x y
      | x > 0          = x
      | y > 0          = y
      | otherwise      = 1/0

    mind = findMinPositive t0 t1
    collisionPoint = movePoint (escalateDir mind d) p1
    vectorNormal = normal $ collisionPoint #< p0

oneCollision (Plane (Plano p0 n color reflec id)) (Ray p1 d m) = (mind, (color, reflec, collisionPoint, vectorNormal, id))
  where
    mind = ((p0 #< p1) .* n) / (d .* n)
    collisionPoint = movePoint (escalateDir mind d) p1
    vectorNormal = normal $ n * d

oneCollision (Triangle (Triangulo p1 p2 p3 color reflec id)) (Ray rayOrigin rayDir m) =
    case rayTriangleIntersection rayOrigin rayDir p1 p2 p3 of
        Just (t, intersectionPoint) ->
            let normalVec = normal $ (p2 #< p1) * (p3 #< p1)
            in (t, (color, reflec, intersectionPoint, normalVec, id))
        Nothing -> (1/0, (color, 0.0, Point3D (1/0) (1/0) (1/0), Direction (1/0) (1/0) (1/0), 0))

getShapeID :: Shape -> Int
getShapeID (Sphere (Esfera _ _ _ _ id)) = id

getShapeID (Plane (Plano _ _ _ _ id)) = id

getShapeID (Triangle (Triangulo _ _ _ _ _ id)) = id

rayTriangleIntersection :: Point3D -> Direction -> Point3D -> Point3D -> Point3D -> Maybe (Float, Point3D)
rayTriangleIntersection orig dir v1 v2 v3 = do
    let e1 = v2 #< v1
        e2 = v3 #< v1
        h = dir * e2
        a = e1 .* h
    if a > (-1e-6) && a < 1e-6
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
parsePoint3D :: String -> Maybe Point3D
parsePoint3D line = case words line of
    ["v", xStr, yStr, zStr] -> Just $ Point3D (read xStr) (read yStr) (read zStr)
    _ -> Nothing

-- Parse a line of the .obj file into a Triangle
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
vertexToPoint3D :: Point3D -> Point3D
vertexToPoint3D (Point3D x y z) = Point3D (realToFrac x) (realToFrac y) (realToFrac z)

-- Convert Triangle to Triangulo
triangleToTriangulo :: ([Point3D], TrianglePos) -> Shape
triangleToTriangulo (vertices, TrianglePos v1 v2 v3) =
    Triangle(Triangulo
        (vertices !! (v1 - 1)) (vertices !! (v2 - 1)) (vertices !! (v3 - 1))
        (RGB 0 0 255) 0 0
    )
-- Convert loaded vertices and triangles to custom format
convertToCustomFormat :: ([Point3D], [TrianglePos]) -> [Shape]
convertToCustomFormat (vertices, triangles) = map (triangleToTriangulo.resolveVertices) triangles
  where
    resolveVertices (TrianglePos v1 v2 v3) = (vertices, TrianglePos v1 v2 v3)
