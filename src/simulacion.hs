import Elem3D
import Files
import Figuras
import Data.List
import Data.Maybe
import Control.Parallel.Strategies (using, rseq, parListChunk)
import Control.DeepSeq (force)
-- make clean && make sim && ./sim -N && convert a.ppm out.png

generateRaysForPixels :: Point3D -> Int -> Int -> Float -> [Ray] --Cambiar de tres valores a un solo punto
generateRaysForPixels p width height focalLength =
  [Ray p (generateDirection (fromIntegral x) (fromIntegral y) focalLength) 10 | y <- [(-(width `div` 2))..((width `div` 2)-1)], x <- [(-(height `div` 2))..((height `div` 2)-1)]]
  where
      generateDirection :: Float -> Float -> Float -> Direction
      generateDirection width height focal = normal ((Point3D width height focal) #< p)

intToPixCoord :: Int -> [Int] -> [(Int,Int)]
intToPixCoord w values = map (\a -> (mod a w, div a w)) values

filterPositionsOfNotEmpty :: Int -> [Maybe a]  -> [(Int,Int)]
filterPositionsOfNotEmpty w lista = (intToPixCoord w) . elemIndices True $ boolList
  where
    boolList = map isJust lista

filterPositionsOfEmpty :: Int -> [Float]  -> [(Int,Int)]
filterPositionsOfEmpty w lista = intToPixCoord w roundedLista
  where
  roundedLista = map round lista

recenter :: Int -> Int -> (Int, Int) -> (Int, Int)
recenter w h (x, y) = (x+w, y+h)

-- Function to find the tuple with the minimum Float value
findMinTuple :: (Float, RGB) -> (Float, RGB) -> RGB
findMinTuple (a, r0) (b, r1) = if a <= b then r0 else r1

-- Función principal
listRayToRGB :: [[(Float, RGB)]] -> [RGB]
listRayToRGB listaDeListas = map obtenerRGBMinimo (transpose listaDeListas)
  where
    obtenerRGBMinimo :: [(Float, RGB)] -> RGB
    obtenerRGBMinimo = snd . foldl1' (\acc@(minFloat, _) (x, rgb) -> if (x < minFloat && x >= 0) then (x, rgb) else acc)

pix :: Int
pix = 720
pixF :: Float
pixF = 720.0
centr = Point3D (125) (150) 0
centr' = Point3D (-50) 200 0
triangulo = Triangulo (Point3D (5) (25) 70) (Point3D (15) (5) 70) (Point3D (5) (5) 70) (RGB 255 0 255)

plano0 = Plano (Point3D (-200) 0 500) (Direction 1 0 0) (RGB 249 176 84)
plano1 = Plano (Point3D (200) 0 500) (Direction (1) (0) (0)) (RGB 146 223 222)
plano2 = Plano (Point3D 0 (200) 500) (Direction 0 (-1) 0) (RGB 0 255 0)
plano3 = Plano (Point3D 0 0 500) (Direction 0 0 (-1)) (RGB 175 170 169)
plano4 = Plano (Point3D 0 (-250) 500) (Direction 0 (-1) (0)) (RGB 175 170 169)
bola = Esfera centr 50 (RGB 255 0 0)
bola' = Esfera centr' 40 (RGB 0 0 255)
camara = Point3D (0) (0) (-500)
rayitos = generateRaysForPixels camara pix pix 1.0

main :: IO ()
main = do
      let sol = force map (parametricSphereCollision bola') rayitos `using` parListChunk 32 rseq
      let sol' = force map (parametricSphereCollision bola) rayitos `using` parListChunk 32 rseq
      let sol0 = force map (parametricPlaneCollision plano0) rayitos `using` parListChunk 32 rseq
      let sol1 = force map (parametricPlaneCollision plano1) rayitos `using` parListChunk 32 rseq
      let sol2 = force map (parametricPlaneCollision plano2) rayitos `using` parListChunk 32 rseq
      let sol3 = force map (parametricPlaneCollision plano3) rayitos `using` parListChunk 32 rseq
      let sol4 = force map (parametricPlaneCollision plano4) rayitos `using` parListChunk 32 rseq
      let sol'' = force map (parametricTriangleCollision triangulo) rayitos `using` parListChunk 32 rseq
      let a = concat $ map rgbToString . listRayToRGB $ [sol, sol',sol0,sol1,sol2, sol3,sol4]
      -- let a = concat $ map rgbToString . listRayToRGB $ [sol'', sol3]
      --let ppm = concat (map (rgbToString.(\(x, y) -> findMinTuple x y)) listaIntersecciones)
      -- let ppm' = fromPixToList pix pix (filteredPositions ++ (filterPositionsOfEmpty pix sol))
      writePPM "a.ppm" pix pix a
      -- mapM_ (\(floatVal, _) -> print floatVal) sol2
      -- print filteredPositions
      -- writeBMP "a.bmp" pix pix pixels
      
