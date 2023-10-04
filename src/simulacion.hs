-- import Base
import Elem3D
import Files
import Figuras
-- import Control.Monad
import qualified Data.ByteString as BS
import Data.List (elemIndices)
import Data.Maybe
import Control.Parallel.Strategies (using, rseq, parListChunk)
import Control.DeepSeq (force)
-- hacerFoto  :: Float -> Float -> [Point3D] -> [(Int,Int)]
-- hacerFoto x y puntos3D = map ((floatTupleToIntTuple).(project x y (-30))) puntos3D

-- foto :: [(Int,Int)]
-- foto = hacerFoto (-200) (-200) (ballInstance ++ cubeInstance)

-- -- filledpixels' :: [(Int,Int)]
-- -- filledpixels' = ballgenerator (-300) (-) 150

-- -- filledpixels'' :: [(Int,Int)]
-- -- filledpixels'' = ballgenerator (-200) (-200) 60

-- allpixels :: BS.ByteString
-- allpixels = generateCustomPixelData 400 400 (foto)

generateRaysForPixels :: Point3D -> Int -> Int -> Float -> [Ray]
generateRaysForPixels p width height focalLength =
  [Ray p (generateDirection (fromIntegral x) (fromIntegral y) focalLength) 10 | x <- [0..(width-1)], y <- [0..(height-1)]]
  where
      generateDirection :: Float -> Float -> Float -> Direction
      generateDirection width height focal = (Point3D width height focal) #< p

intToPixCoord :: Int -> [Int] -> [(Int,Int)]
intToPixCoord w values = map (\a -> (mod a w, div a w)) values

filterPositionsOfNotEmpty :: Int -> [Maybe a]  -> [(Int,Int)]
filterPositionsOfNotEmpty w lista = (intToPixCoord w) . elemIndices True $ boolList
  where
    boolList = map isJust lista


recenter :: Int -> Int -> (Int, Int) -> (Int, Int)
recenter w h (x, y) = (x+w, y+h)

-- Función para imprimir la solución
--printSolution :: Maybe (Float) -> String
--printSolution Nothing = ""
--printSolution (Just (x1)) = show x
pix :: Int
pix = 1000
pixF :: Float
pixF = 1000.0
centr = Point3D (pixF/4) (pixF/4) 100
bola = Esfera centr 32
camara = Point3D 0 0 0
rayitos = generateRaysForPixels camara pix pix 300.0

main :: IO ()
main = do
      let sol = force map (parametricSphereCollision bola) rayitos `using` parListChunk 32 rseq
      let filteredPositions = filterPositionsOfNotEmpty pix sol
      let pixels = force generateBMPPixelData pix pix filteredPositions
      let ppm = generatePPMPixelData sol
      writePPM "a.ppm" pix pix ppm
      -- print filteredPositions
      -- writeBMP "a.bmp" pix pix pixels
      
