{-# LANGUAGE BangPatterns #-}
import Elem3D
import Files
import Figuras
import Data.List
import Data.Ord
import Data.Maybe
import Control.Parallel.Strategies (using, rseq, parListChunk)
import Debug.Trace
import Data.List (any)
import System.CPUTime
import qualified Data.Vector as V
-- make clean && make sim && ./sim -N && convert a.ppm out.bmp

generateRaysForPixels :: Camara -> Float -> Float -> [Ray]
generateRaysForPixels (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height =
  [Ray p (generateDirection x y focal) 10 | y <- [(-py'), (piY-py') ..(py'-piY)], x <- [(-px'), (piX-px') ..(px'-piX)]]
  where
      piY = py / width
      piX = px / height
      px' = px / 2
      py' = py / 2
      generateDirection width height focal = normal ((Point3D width height focal) #< p)

obtenerPrimeraColision :: [(Float, (RGB,Float, Point3D, Direction, Int))] -> (Float,(RGB,Float, Point3D, Direction,Int))
obtenerPrimeraColision = minimumBy (comparing fst) . filter (\(x, _) -> x >= 0)

-- dada una lista de colisiones devuelve la lista de puntos
obtenerPuntos :: [(Float, (RGB,Float, Point3D, Direction, Int))] -> [Point3D]
obtenerPuntos lista = map (\(_, (_, _,point, _,_)) -> point) lista

-- dada la matriz de [figuras,colisiond e cada rayo] devuelve la lista de la primera colisión de cada rayo
listRay :: [[(Float, (RGB, Float, Point3D, Direction, Int))]] -> [(Float, (RGB, Float, Point3D, Direction, Int))]
listRay = map obtenerPrimeraColision . transpose

calcularDirESpejo :: Direction -> Direction -> Direction
calcularDirESpejo d normal = d - (escalateDir (2 * (d .* normal)) normal)

listRayToRGB :: Point3D -> Point3D -> [Shape] -> [[(Float, (RGB, Float, Point3D, Direction, Int))]] -> [RGB]
listRayToRGB luz cam figuras listaDeColisiones = b
  where
    !parametricfiguras = parametricShapeCollision figuras
    !ligthRays = [Ray luz (punto #< luz) 0 | punto <- obtenerPuntos (listRay listaDeColisiones)]
    
    !collisions = parametricfiguras ligthRays `using` parListChunk 16 rseq
    
    !luzXRayo = zipWith eligeResultado (listRay listaDeColisiones) (listRay collisions)
      where
        eligeResultado a@(t, (rgb1@(RGB r g b), ra, pa, d, id)) (_, (_, _, pb, _, _))
          | aproxPoint pa pb = a
          | otherwise = (t, (RGB (r / 3) (g / 3) (b / 3), ra, pa, d, id))
    
    !b = map (\(_, (rgb, _, _, _, _)) -> rgb) $ map (oneEspejo cam figuras) luzXRayo
      where
        oneEspejo :: Point3D -> [Shape] -> (Float, (RGB, Float, Point3D, Direction, Int)) -> (Float, (RGB, Float, Point3D, Direction, Int))
        oneEspejo cam shapes esp@(f, (rgb, ref, p, d, id))
          | ref == 0 = esp
          | otherwise = let
              otherShapes = filter (\shape -> not (id == getShapeID shape)) shapes
              !cortes = map (\shape -> oneCollision shape (Ray p (calcularDirESpejo (p #< cam) d) 0)) otherShapes
              rgbRefle = (\(_, (rgb, _, _, _, _)) -> rgb) $ obtenerPrimeraColision cortes
              newRgb = agregateRGBPoints (rgbProd rgb (1 - ref)) (rgbProd rgbRefle ref)
            in (f, (newRgb, ref, p, d, id))
              
      -- a = map b
      {- where
        -- calcular rayo de punto de colisión a luz, su distancia y angulo
        ligthDir = (luz #< collisionPoint)
        ligthRay =  Ray collisionPoint ligthDir dluz
        dluz = modd ligthDir
        angle = (normal .* ligthDir) / ((modd normal) * (modd ligthDir))
        reflectedDir = (cam #< collisionPoint)
        -- (reflectedPointRGB, _, _, _) =  (snd.obtenerPrimeraColision.head) (transpose (figuras [(Ray collisionPoint reflectedDir 2)]))
        -- reflectedRGB = agregateRGBPoints (rgbProd rgb (1 - ref)) (rgbProd reflectedPointRGB ref)
        reflectedRGB = rgb
        newRgb = if not colisiona then rgbProd reflectedRGB (1 - (angle / 90.0)) else rgbProd reflectedRGB (1/3) -}


pix :: Float
pix = 2048
piCam :: Float
piCam = 250
basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 (-500))
centr = Point3D (0) (100) 0
centr' = Point3D (-50) 200 0
centr'' = Point3D (100) (200) (-20)
triangulo = Triangulo (Point3D (5) (25) 70) (Point3D (15) (5) 70) (Point3D (5) (5) 70) (RGB 255 0 255)
luz = Point3D (150) (0) (0)
cam' =  Point3D (0) (0) (-1000)
plano0 = Plane (Plano (Point3D (-200) 0 200) (Direction 1 0 0) (RGB 249 176 84) 0 0)
plano1 =  Plane (Plano (Point3D (200) 0 200) (Direction (1) (0) (0)) (RGB 146 223 222) 0 1)
plano2 =  Plane (Plano (Point3D 0 (200) 200) (Direction 0 (-1) 0) (RGB 0 255 0) 0 2)
plano3 =  Plane (Plano (Point3D 0 0 200) (Direction 0 0 (-1)) (RGB 255 255 255) 0 3)
plano4 =  Plane (Plano (Point3D 0 (-250) 200) (Direction 0 (-1) (0)) (RGB 255 0 255) 0 4)
plano5 =  Plane (Plano (Point3D 0 0 (-1001)) (Direction 0 0 (1)) (RGB 255 255 0) 0 3)
bola =  Sphere (Esfera centr 50 (RGB 255 0 0) 1 5)
bola' =  Sphere (Esfera centr' 40 (RGB 0 0 255) 0 6)
bola'' =  Sphere (Esfera centr'' 50 (RGB 155 0 155) 1 7)
tri1 = Triangle (Triangulo (Point3D 0 100 50) (Point3D 100 100 50) (Point3D 100 0 100) (RGB 60 80 100) 0 8)
bolaLus = Sphere (Esfera luz 10 (RGB 255 255 255) 0 8)
camara = Camara (Point3D (0) (0) (-1000)) basCam


figuras = [bola,bola',bola'',plano0,plano1,plano2,plano3,plano4,plano5, tri1]

-- figurasSinPlanos = (parametricShapeCollision [bola,bola',bola''])
main :: IO ()
main = do

      let objFilePath = "cubo.obj"  -- Replace with the path to your .obj file
      (vertices, triangles) <- loadObjFile objFilePath
      let customTriangles = convertToCustomFormat (vertices, triangles)
      let figuras' = figuras ++ customTriangles
      start <- getCPUTime

      let rayitos = generateRaysForPixels camara pix pix --`using` parListChunk 128 rseq
      let !sol = parametricShapeCollision figuras' rayitos --`using` parListChunk 128 rseq
      let a = concat $ map rgbToString . (listRayToRGB luz cam' figuras') $ sol
      -- let solBolaLus =  force parametricShapeCollision [bolaLus] rayitos
      -- let solo = listRay sol
      -- let lusesita = force map (\punto -> Ray luz (punto #< luz) 0) $ obtenerPuntos solo 
      -- let solLus = force parametricShapeCollision [bola,bola',plano0,plano1,plano2,plano3,plano4] lusesita `using` parListChunk 32 rseq
      -- let sol' = luzXRayo (solo) (listRay solLus)
      -- let sol'' = listRay [sol',(concat solBolaLus)]
      -- let a = map rgbToString $ map (\(_,(rgb,_,_,_)) -> rgb) sol''
      -- writePPM "a.ppm" (round pix) (round pix) (concat a)
      
      writePPM "a.ppm" (round pix) (round pix) a
      
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Double
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"
