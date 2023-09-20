import System.IO ()
import Graphics.Gloss.Geometry.Angle
import System.Random

data Point3D = Point3D Float Float Float
data Direction = Direction Float Float Float
data Base = Base Direction Direction Direction

instance Show Base where
    show (Base (Direction dx0 dy0 dz0) (Direction dx1 dy1 dz1) (Direction dx2 dy2 dz2)) = "Base:\n| " ++ show dx0 ++" " ++show dy0 ++" "++ show dz0 ++ " |\n" ++ "| " ++ show dx1 ++" " ++show dy1 ++" "++ show dz1 ++ " |\n"++ "| " ++ show dx2 ++" " ++show dy2 ++" "++ show dz2 ++ "| "

instance Show Point3D where
    show (Point3D x y z) = "Point3D " ++ show x ++ " " ++ show y ++ " " ++ show z

instance Show Direction where
    show (Direction dx dy dz) = "Direction " ++ show dx ++ " " ++ show dy ++ " " ++ show dz

roundTo :: Int -> Point3D -> Point3D
roundTo n (Point3D a b c) = Point3D a' b' c'
    where 
        a' = (fromInteger $ round $ a * (10^n)) / (10.0^^n)::Float
        b' = (fromInteger $ round $ b * (10^n)) / (10.0^^n)::Float
        c' = (fromInteger $ round $ c * (10^n)) / (10.0^^n)::Float


roundTo5 = roundTo 5

radiansToDegrees :: Float -> Float
radiansToDegrees radians = radians * (180.0 / pi)

rotatePoint :: Char -> Float -> Point3D -> Point3D
rotatePoint axis degree  (Point3D x y z)
 | axis == 'X' = Point3D x (c*y + s*z) (-s*y + c*z)
 | axis == 'Y' = Point3D (-s*z + c*x) y (s*x + c*z)
 | axis == 'Z' = Point3D (c*x + s*y) (-s*x + c*y) z
 | otherwise = Point3D 0 0 0
    where radiant = normalizeAngle $ degToRad degree
          c = cos radiant
          s = sin radiant

rotatePoint' :: Char -> Float -> Point3D -> Point3D
rotatePoint' axis degree  (Point3D x y z)
 | axis == 'X' = Point3D x (c*y + s*z) (-s*y + c*z)
 | axis == 'Y' = Point3D (-s*z + c*x) y (s*x + c*z)
 | axis == 'Z' = Point3D (c*x + s*y) (-s*x + c*y) z
 | otherwise = Point3D 0 0 0
    where radiant = normalizeAngle $ degToRad (360 - degree)
          c = cos radiant
          s = sin radiant



movePoint :: Direction -> Point3D -> Point3D
movePoint (Direction dx dy dz) (Point3D x y z) = Point3D (x + dx) (y + dy) (z + dz)

movePoint' :: Direction -> Point3D -> Point3D
movePoint' (Direction dx dy dz) (Point3D x y z) = Point3D (x - dx) (y - dy) (z - dz)
 
-- -- Resta de puntos -> Dirección del primero al segundo
(#) :: Point3D -> Point3D -> Direction
(Point3D xb yb zb) # (Point3D xa ya za)  = Direction (xb-xa) (yb-ya) (zb-za)

instance Num Direction where
-- Suma de direcciones
--  (+) :: Direction -> Direction -> Direction
 (Direction x1 y1 z1) + (Direction x2 y2 z2) = Direction (x1 + x2) (y1 + y2) (z1 + z2)
 
 -- Producto vectorial
--  (*) :: Direction -> Direction -> Direction
 Direction xa ya za * Direction xb yb zb = Direction (ya*zb-za*yb) (za*xb-xa*zb) (xa*yb-ya*xb)

-- -- -- Producto escalar
(.*) :: Direction -> Direction -> Float
(Direction xb yb zb) .* (Direction xa ya za)  = (xb*xa + yb*ya + zb*za)

-- -- -- Escalado de dirección
escalateDir :: Float -> Direction -> Direction
escalateDir s (Direction xa ya za) = Direction (s*xa) (s*ya) (s*za)

-- -- -- Escalado de puntos
escalatePoint :: Float -> Point3D -> Point3D
escalatePoint s (Point3D xa ya za) = (Point3D (s*xa) (s*ya) (s*za))

-- -- -- Escalado de dirección
escalateDir' :: Float -> Direction -> Direction
escalateDir' s (Direction xa ya za) = Direction (s/xa) (s/ya) (s/za)

-- -- -- Escalado de puntos
escalatePoint' :: Float -> Point3D -> Point3D
escalatePoint' s (Point3D xa ya za) = (Point3D (s/xa) (s/ya) (s/za))


-- -- Modulo
modd :: Direction -> Float
modd (Direction xb yb zb) = sqrt((xb**2) + (yb**2) + (zb**2))

-- Normalización
normal :: Direction -> Direction
normal (Direction xb yb zb)
    | bmod == 0 = Direction 0 0 0
    | otherwise = (Direction (xb / bmod) (yb / bmod) (zb / bmod))
    where
        bmod = modd (Direction xb yb zb)

generateBase :: Base
generateBase = Base (Direction 1 0 0) (Direction 0 1 0) (Direction 0 0 1)

cambioBase :: Direction  -> Base  -> [Point3D] -> [Point3D]
cambioBase nuevoOrigen (Base nuevoEjeX nuevoEjeY nuevoEjeZ) elementoACambiarDeBase = elementoDeBaseCambiada
    where
        elementoDeBaseCambiada = map (roundTo5.(rotatePoint 'Z' thz).(rotatePoint 'Y' thy).(rotatePoint 'X' thx).(movePoint nuevoOrigen)) elementoACambiarDeBase
        dx = Direction 1 0 0
        dy = Direction 0 1 0
        dz = Direction 0 0 1
        thx = radiansToDegrees (acos ((nuevoEjeX .*  dx) / (modd nuevoEjeX * modd dx))) 
        thy = radiansToDegrees (acos ((nuevoEjeY .*  dy) / (modd nuevoEjeY * modd dy)))
        thz = radiansToDegrees (acos ((nuevoEjeZ .*  dz) / (modd nuevoEjeZ * modd dz)))


cambioBase' :: Direction  -> Base  -> [Point3D] -> [Point3D]
cambioBase' nuevoOrigen (Base nuevoEjeX nuevoEjeY nuevoEjeZ) elementoACambiarDeBase = elementoDeBaseCambiada
    where
        elementoDeBaseCambiada = map (roundTo5.(rotatePoint' 'Z' thz).(rotatePoint' 'Y' thy).(rotatePoint' 'X' thx).(movePoint' nuevoOrigen)) elementoACambiarDeBase
        dx = Direction 1 0 0
        dy = Direction 0 1 0
        dz = Direction 0 0 1
        thx = radiansToDegrees (acos ((nuevoEjeX .*  dx) / (modd nuevoEjeX * modd dx))) 
        thy = radiansToDegrees (acos ((nuevoEjeY .*  dy) / (modd nuevoEjeY * modd dy)))
        thz = radiansToDegrees (acos ((nuevoEjeZ .*  dz) / (modd nuevoEjeZ * modd dz)))

polarToCartesian :: Float -> Float -> Float -> Point3D
polarToCartesian theta gamma radius = (Point3D x y z)
    where thetaRad = degToRad theta
          gammaRad = degToRad gamma
          sinTheta = sin thetaRad
          x = radius * sinTheta * cos gammaRad
          y = radius * sinTheta * sin gammaRad
          z = radius * cos thetaRad

ciudad :: Float -> ([Point3D],(Float,Float))
ciudad rad = (map (\(t, g) -> polarToCartesian t g rad) randomAnglePair),(head theta, head gamma))
      where   
            randomAnglePair = zip thetas gammas
            thetas = take 1 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42
            gammas = take 1 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337
                 
-- De alguna forma con la theta y la gamma buscas la Normal y la tangente y su producto vectorial
--tangente es normal entre radio y ciudad
p1 = ciudad 200
p2 = ciudad 10

a = Point3D 3 3 3
b = Point3D 5 5 5
res = b # a
bas = generateBase
c = Direction 1 1 1
d = Direction 3 3 3
res2 = c + d
res4 = c .* d


main :: IO ()
main = do
    print a
    print (head a')
    print (head a'')
    where
        nuevaBase = (Base (Direction 1 2 0)  (Direction (-1) 1 1) (Direction 0 1 (-1)))
        nuevoOrigen = (Direction 0 0 0) 
        a' = cambioBase nuevoOrigen nuevaBase [a]
        a'' = cambioBase' nuevoOrigen nuevaBase a'