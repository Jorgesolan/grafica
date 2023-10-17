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

sumRGB :: (Float, (RGB, Float, Point3D, Direction, Int)) -> (Float, (RGB, Float, Point3D, Direction, Int)) -> (Float, (RGB, Float, Point3D, Direction, Int))
sumRGB (f, (RGB r g b, fl, p, d, id)) (_, (RGB r' g' b', _, _, _, _)) = (f, (RGB (r + r') (g + g') (b + b'), fl, p, d, id))

mediaRGB :: [(Float, (RGB, Float, Point3D, Direction, Int))] -> Float -> (Float, (RGB, Float, Point3D, Direction, Int))
mediaRGB lista n = (medRGB (1/n)) (foldr sumRGB (head lista) (tail lista))
  where
    medRGB :: Float -> (Float, (RGB, Float, Point3D, Direction, Int)) -> (Float, (RGB, Float, Point3D, Direction, Int))
    medRGB n (f, (RGB r g b, fl, p, d, id)) = (f, (RGB ( r * n) ( g * n) ( b * n), fl, p, d, id))

mediaDeRayos :: [[(Float, (RGB, Float, Point3D, Direction, Int))]] -> [(Float, (RGB, Float, Point3D, Direction, Int))]
mediaDeRayos = map (\rayos -> mediaRGB rayos (fromIntegral(length rayos))) . transpose

listRayToRGB :: [Point3D] -> Point3D -> [Shape] -> [[(Float, (RGB, Float, Point3D, Direction, Int))]] -> [(Float, (RGB, Float, Point3D, Direction, Int))]
listRayToRGB luces cam figuras listaDeColisiones = b
  where
    !parametricfiguras = parametricShapeCollision figuras
    !ligthRays = [[Ray luz (punto #< luz) 0 | punto <- obtenerPuntos (listRay listaDeColisiones)] | luz <- luces]
    
    !collisions = [parametricfiguras ligthRay | ligthRay <- ligthRays]
    
    !luzXRayo = [zipWith eligeResultado (listRay listaDeColisiones) (listRay collision) | collision <- collisions]
      where
        eligeResultado a@(t, (_, ra, pa, d, id)) (_, (_, _, pb, _, _))
          | aproxPoint pa pb = a
          | otherwise = (t, (RGB 0 0 0, ra, pa, d, id))
    
    !mediaLuzxRayo = mediaDeRayos luzXRayo

    !b = map (oneEspejo cam figuras) (mediaDeRayos luzXRayo)
      where
        oneEspejo :: Point3D -> [Shape] -> (Float, (RGB, Float, Point3D, Direction, Int)) -> (Float, (RGB, Float, Point3D, Direction, Int))
        oneEspejo cam shapes esp@(f, (rgb, ref, p, d, id))
          | ref == 0 = esp
          | otherwise = let
              otherShapes = filter (\shape -> not (id == getShapeID shape)) shapes
              !cortes = map (\shape -> oneCollision shape (Ray p (calcularDirESpejo (p #< cam) d) 0)) otherShapes
              rgbRefle = (\(_, (rgb, _, _, _, _)) -> rgb) $ obtenerPrimeraColision cortes
              newRgb = agregateRGBPoints (rgbProd (1 - ref) rgb ) (rgbProd ref rgbRefle)
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
pix = 4096
piy :: Float
piy = 4096
piCamX :: Float
piCamX = 250
piCamY :: Float
piCamY = 250
basCam = Base (Direction piCamX 0 0) (Direction 0 piCamY 0) (Direction 0 0 (-500))
centr = Point3D (0) (100) 0
centr' = Point3D (-50) 200 0
centr'' = Point3D (100) (200) (-20)
luz = Point3D (150) (0) (-20)
luz' = Point3D (0) (-150) (-20)
luz'' = Point3D (100) (140) (-20)
cam' =  Point3D (0) (0) (-1000)
plano0 = Plane (Plano (Point3D (-200) 0 200) (Direction 1 0 0) (RGB 249 176 84) 0 0)
plano1 =  Plane (Plano (Point3D (200) 0 200) (Direction (1) (0) (0)) (RGB 146 223 222) 0 1)
plano2 =  Plane (Plano (Point3D 0 (200) 200) (Direction 0 (-1) 0) (RGB 0 255 0) 0 2)
plano3 =  Plane (Plano (Point3D 0 0 200) (Direction 0 0 (-1)) (RGB 171 118 24) 0 3)
plano4 =  Plane (Plano (Point3D 0 (-250) 200) (Direction 0 (-1) (0)) (RGB 255 0 255) 0 4)
plano5 =  Plane (Plano (Point3D 0 0 (-1001)) (Direction 0 0 (1)) (RGB 255 255 0) 0 3)
bola =  Sphere (Esfera centr 50 (RGB 255 0 0) 1 5)
bola'' =  Sphere (Esfera centr'' 50 (RGB 155 0 155) 1 7)
-- tri1 = Triangle (Triangulo (Point3D 0 100 50) (Point3D 100 100 50) (Point3D 100 0 100) (RGB 60 80 100) 0 8)
bolaLus = Sphere (Esfera luz 10 (RGB 255 0 255) 0 8)
camara = Camara (Point3D (0) (0) (-1000)) basCam

generarBolaLuz :: Point3D -> Shape
generarBolaLuz p = Sphere (Esfera p 10 (RGB 255 255 255) 0 8)

partirEnCuatro :: [a] -> Maybe ([a], [a], [a], [a])
partirEnCuatro lista
  | length lista `mod` 4 == 0 = Just (take n lista, take n (drop n lista), take n (drop (2 * n) lista), drop (3 * n) lista)
  | otherwise = Nothing
  where
    n = length lista `div` 4

figuras = [bola,bola'',plano0,plano1,plano2,plano3,plano4,plano5]
luces = [luz,luz',luz'']
bolasLuz = map generarBolaLuz luces
-- figurasSinPlanos = (parametricShapeCollision [bola,bola',bola''])
main :: IO ()
main = do
      start <- getCPUTime 
      let objFilePath = "diamante.obj"  -- Replace with the path to your .obj file
      (vertices, triangles) <- loadObjFile objFilePath
      let !vertices' = map (rotatePoint 'Y' (0).movePoint (Direction (-50) (-25) 0).escalatePoint (25)) vertices
      let !customTriangles = convertToCustomFormat (vertices', triangles)

      let objFilePath1 = "cubo.obj"  -- Replace with the path to your .obj file
      (vertices1, triangles1) <- loadObjFile objFilePath1
      let !vertices1' = map (rotatePoint 'X' (0).movePoint (Direction (-50) (0) 0).escalatePoint (25)) vertices1
      let !customTriangles1 = convertToCustomFormat (vertices1', triangles1)
      let !figuras' = figuras ++ customTriangles ++ customTriangles1

      let rayitos = generateRaysForPixels camara pix piy --`using` parListChunk 128 rseq
      let !sol = parametricShapeCollision figuras' rayitos --`using` parListChunk 128 rseq
      let !a = (listRayToRGB luces cam' figuras') $ sol
      let !representacionLuces = parametricShapeCollision bolasLuz rayitos
      let !fin = concat $ map rgbToString . map (\(_, (rgb, _, _, _, _)) -> rgb) $ listRay [a,(listRay representacionLuces)]
      --     case partirEnCuatro fin of
      -- Just (parte1, parte2, parte3, parte4) -> do
      --   writeToList "parte1.ppm" parte1
      --   writeToList "parte2.ppm" parte2
      --   writeToList "parte3.ppm" parte3
      --   writeToList "parte4.ppm" parte4
      -- Nothing ->
      --   putStrLn "La longitud de la lista es menor a 4."
      
      writePPM "a.ppm" (round pix) (round piy) fin
      
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Double
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"
