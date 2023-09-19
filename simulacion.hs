import Base
import Files

import qualified Data.ByteString as BS
import System.Random


sphere :: Float -> [Point3D]
sphere rad = map (\(t, g) -> polarToCartesian t g rad) randomAnglePairs
      where   
            randomAnglePairs = zip thetas gammas
            thetas = take 100 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42
            gammas = take 100 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337


-- recenter :: Int -> Int -> (Int, Int) -> (Int, Int)
-- recenter w h (x, y) = (x-w, y-h)

ballgenerator :: Float -> Float -> Float -> [(Int,Int)]
ballgenerator x y radius = map ((floatTupleToIntTuple).(project (x) (y) (-25)).(movePoint (0.0, 0.0, 200.0))) (sphere radius)

filledpixels :: [(Int,Int)]
filledpixels = ballgenerator (-100) (-300) 150

filledpixels' :: [(Int,Int)]
filledpixels' = ballgenerator (-300) (-100) 150

filledpixels'' :: [(Int,Int)]
filledpixels'' = ballgenerator (-200) (-200) 60


allpixels :: BS.ByteString
allpixels = generateCustomPixelData 400 400 (filledpixels ++ filledpixels' ++ filledpixels'')

main :: IO ()
main = do
      -- print (showDirection (concatDirections (3,3,3) (4,4,4)))
      -- animate window black frame
      -- print ( )
      writeBMP "custom_image.bmp" 400 400 allpixels

