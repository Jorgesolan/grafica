import Elem3D
import Files
import Figuras
import Data.List
import Data.Maybe
import Control.Parallel.Strategies (using, rseq, parListChunk)
import Control.DeepSeq (force)
import Debug.Trace
import Data.List (any)
import System.CPUTime
-- make clean && make sim && ./sim -N && convert a.ppm out.bmp

generateRaysForPixels :: Camara -> Float -> Float -> [Ray]
generateRaysForPixels (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height =
  [Ray p (generateDirection x y focal) 10 | y <- [(-py'), (piY-py') ..(py'-piY)], x <- [(-px'), (piX-px') ..(px'-piX)]]
  where
      piY = py / width
      piX = px / height
      px' = px / 2
      py' = py / 2
      generateDirection :: Float -> Float -> Float -> Direction
      generateDirection width height focal = normal ((Point3D width height focal) #< p)

-- intToPixCoord :: Int -> [Int] -> [(Int,Int)]
-- intToPixCoord w values = map (\a -> (mod a w, div a w)) values

-- filterPositionsOfNotEmpty :: Int -> [Maybe a]  -> [(Int,Int)]
-- filterPositionsOfNotEmpty w lista = (intToPixCoord w) . elemIndices True $ boolList
--   where
--     boolList = map isJust lista

-- filterPositionsOfEmpty :: Int -> [Float]  -> [(Int,Int)]
-- filterPositionsOfEmpty w lista = intToPixCoord w roundedLista
--   where
--   roundedLista = map round lista

-- recenter :: Int -> Int -> (Int, Int) -> (Int, Int)
-- recenter w h (x, y) = (x+w, y+h)

-- listRayToRGB ::  Point3D -> (Ray -> (Float,(RGB,Point3D,Direction))) -> [[(Float, (RGB, Point3D, Direction))]] -> [RGB]
-- listRayToRGB luz figuras listaDeListas = trace (show (length listaDeListas)) $ map (calcularColor.obtenerRGBMinimo) (transpose listaDeListas)
--   where
--     obtenerRGBMinimo :: [(Float, (RGB, Point3D, Direction))] -> (RGB, Point3D, Direction)
--     obtenerRGBMinimo = snd . foldl1' (\acc@(minFloat,( _, _, _)) (x, (rgb, collisionPoint, normal)) -> if (x < minFloat && x >= 0) then (x, (rgb, collisionPoint, normal)) else acc)
--     calcularColor (rgb,collisionPoint,normal) = trace (show colisiona) $ if not colisiona then newRgb else (RGB 0 0 0)  -- Return a default value when there's no collision
--       where
--         ligthDir = (luz #< collisionPoint)
--         ligthRay =  Ray collisionPoint ligthDir 300
        
--         -- lanzar rauo desde p colision hasta luz mirando si en medio hay alguna intersec con figura
--         --(falla por empty list salvo q uses el let a comentado con la luz dentro de una esfera en  00 0??)
--         -- collisions = figuras ligthRay
--         -- colisiona = (fst collisions) < (1/0)
--         colisiona = (angle >= 90.0) || (angle < (-90.0))
--         angle = (normal .* ligthDir) / ((modd normal) * (modd ligthDir))
--         newRgb = rgbProd rgb (1 - (angle / 90.0))

luzXRayo :: [(Float, (RGB, Point3D, Direction))] -> [(Float, (RGB, Point3D, Direction))] -> [(Float, (RGB, Point3D, Direction))]
luzXRayo = zipWith eligeResultado
  where
  eligeResultado :: (Float, (RGB, Point3D, Direction)) -> (Float, (RGB, Point3D, Direction)) -> (Float, (RGB, Point3D, Direction))
  eligeResultado a@(t, (rgb1@(RGB r g b), pa, d)) (_,(_, pb, _))
    | (aproxPoint pa pb) = a
    | otherwise = (t,(RGB (r/3) (g/3) (b/3),pa,d))


listRay ::  [[(Float, (RGB, Point3D, Direction))]] -> [(Float, (RGB, Point3D, Direction))]
listRay listaDeListas = map (obtenerRGBMinimo) (transpose listaDeListas)
  where
    obtenerRGBMinimo :: [(Float, (RGB, Point3D, Direction))] -> (Float, (RGB, Point3D, Direction))
    obtenerRGBMinimo = foldl1' (\acc@(minFloat,( _, _, _)) (x, (rgb, collisionPoint, normal)) -> if (x < minFloat && x >= 0) then (x, (rgb, collisionPoint, normal)) else acc)

listRayToRGB ::  Point3D -> [[(Float, (RGB, Point3D, Direction))]] -> [RGB]
listRayToRGB luz listaDeListas = map (obtainCol. obtenerRGBMinimo) (transpose listaDeListas)
  where
    obtenerRGBMinimo :: [(Float, (RGB, Point3D, Direction))] -> (RGB, Point3D, Direction)
    obtenerRGBMinimo = snd . foldl1' (\acc@(minFloat,( _, _, _)) (x, (rgb, collisionPoint, normal)) -> if (x < minFloat && x >= 0) then (x, (rgb, collisionPoint, normal)) else acc)
    obtainCol (rgb,collisionPoint,normal) = rgb

pix :: Float
pix = 1000
piCam :: Float
piCam = 250
basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 (-500))
centr = Point3D (0) (0) 0
centr' = Point3D (-50) 200 0
triangulo = Triangulo (Point3D (5) (25) 70) (Point3D (15) (5) 70) (Point3D (5) (5) 70) (RGB 255 0 255)
luz = Point3D (100) (-175) (-20)

plano0 = Plane (Plano (Point3D (-200) 0 200) (Direction 1 0 0) (RGB 249 176 84))
plano1 =  Plane (Plano (Point3D (200) 0 200) (Direction (1) (0) (0)) (RGB 146 223 222))
plano2 =  Plane (Plano (Point3D 0 (200) 200) (Direction 0 (-1) 0) (RGB 0 255 0))
plano3 =  Plane (Plano (Point3D 0 0 200) (Direction 0 0 (-1)) (RGB 175 170 169))
plano4 =  Plane (Plano (Point3D 0 (-250) 200) (Direction 0 (-1) (0)) (RGB 255 0 255))
bola =  Sphere (Esfera centr 50 (RGB 255 0 0))
bola' =  Sphere (Esfera centr' 40 (RGB 0 0 255))
bolaLus = Sphere (Esfera luz 10 (RGB 255 255 255))
camara = Camara (Point3D (0) (0) (-1000)) basCam

-- Función para extraer los puntos de las tuplas
obtenerPuntos :: [(Float, (RGB, Point3D, Direction))] -> [Point3D]
obtenerPuntos lista = map (\(_, (_, point, _)) -> point) lista

--figuras = (parametricShapeCollision bola)
main :: IO ()
main = do
      start <- getCPUTime
      let rayitos = generateRaysForPixels camara pix pix `using` parListChunk 32 rseq
      let sol = force parametricShapeCollision [bola,bola',plano0,plano1,plano2,plano3,plano4] rayitos `using` parListChunk 32 rseq
      let solBolaLus =  force parametricShapeCollision [bolaLus] rayitos
      let solo = listRay sol
      let lusesita = force map (\punto -> Ray luz (punto #< luz) 0) $ obtenerPuntos solo 
      let solLus = force parametricShapeCollision [bola,bola',plano0,plano1,plano2,plano3,plano4] lusesita `using` parListChunk 32 rseq
      let sol' = luzXRayo (solo) (listRay solLus)
      let sol'' = listRay [sol',(concat solBolaLus)]
      let a = map rgbToString $ map (\(_,(rgb,_,_)) -> rgb) sol''
      writePPM "a.ppm" (round pix) (round pix) (concat a)
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Double
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"
      -- let a = concat $ map rgbToString . (listRayToRGB luz figuras) $ [sol,sol3]
