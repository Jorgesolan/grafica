import Elem3D
import Files
import Figuras
import Data.List
import Data.Maybe
import Control.Parallel.Strategies (using, rseq, parListChunk)
import Control.DeepSeq (force)
-- make clean && make sim && ./sim -N && convert a.ppm out.bm

generateRaysForPixels :: Point3D -> Int -> Int -> Float -> [Ray] --Cambiar de tres valores a un solo punto
generateRaysForPixels p width height focalLength =
  [Ray p (generateDirection (fromIntegral x) (fromIntegral y) focalLength) 10 | x <- [0..(width-1)], y <- [0..(height-1)]]
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
    obtenerRGBMinimo = snd . foldl1' (\acc@(minFloat, _) (x, rgb) -> if x < minFloat then (x, rgb) else acc)

pix :: Int
pix = 1000
pixF :: Float
pixF = 1000.0
centr = Point3D (pixF*3/4) (pixF*3/4) 1000
centr' = Point3D (pixF/4) (pixF/4) 500
plano0 = Plano (Point3D 0 0 3000) (Direction 1 0 (0)) (RGB 0 0 255)
plano1 = Plano (Point3D 0 0 3000) (Direction (-1) 0 (0)) (RGB 255 0 0)
plano2 = Plano (Point3D 0 0 3000) (Direction 1 1 0) (RGB 0 198 0)
plano3 = Plano (Point3D 0 0 3000) (Direction 0 0 (-1)) (RGB 250 200 100)
bola = Esfera centr 200 (RGB 255 0 0)
bola' = Esfera centr' 50 (RGB 0 0 255)
camara = Point3D 0 0 (-5000)
rayitos = generateRaysForPixels camara pix pix 500.0

main :: IO ()
main = do
      let sol = force map (parametricSphereCollision bola') rayitos `using` parListChunk 32 rseq
      let sol' = force map (parametricSphereCollision bola) rayitos `using` parListChunk 32 rseq
      let sol0 = force map (parametricPlaneCollision plano0) rayitos `using` parListChunk 32 rseq
      let sol1 = force map (parametricPlaneCollision plano1) rayitos `using` parListChunk 32 rseq
      let sol2 = force map (parametricPlaneCollision plano2) rayitos `using` parListChunk 32 rseq
      let sol3 = force map (parametricPlaneCollision plano3) rayitos `using` parListChunk 32 rseq
      --let filteredPositions = force filterPositionsOfNotEmpty pix sol'
      -- let pixels = force generateBMPPixelData pix pix filteredPositions
      -- let ppm = generatePPMPixelData filteredPositions
      let a = concat $ map rgbToString . listRayToRGB $ [sol, sol', sol3]

      --let ppm = concat (map (rgbToString.(\(x, y) -> findMinTuple x y)) listaIntersecciones)
      -- let ppm' = fromPixToList pix pix (filteredPositions ++ (filterPositionsOfEmpty pix sol))
      writePPM "a.ppm" pix pix a
      mapM_ (\(floatVal, _) -> print floatVal) sol2
      -- print filteredPositions
      -- writeBMP "a.bmp" pix pix pixels
      
