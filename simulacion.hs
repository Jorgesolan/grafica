import Base
import Files
-- import Control.Monad
import qualified Data.ByteString as BS
-- import System.Random

sphere :: Float -> [Point3D]
sphere rad = map (\(t, g) -> polarToCartesian t g rad) randomAnglePairs
      where   
            -- randomAnglePairs = zip thetas gammas
            -- -- randomAnglePairs = map floatListToFloatTuple (sequence [thetas, gammas])
            -- thetas = take 1000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42
            -- gammas = take 1000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337
            list1 = [0.0,10.0..360.0]
            list2 = [0.0,10.0..360.0]
            randomAnglePairs = combinateTuples list1 list2                        
            --thetas = [0.0,1.0 .. 360.0]
            --gammas = [0.0, 0.1 .. 360.0]


-- recenter :: Int -> Int -> (Int, Int) -> (Int, Int)
-- recenter w h (x, y) = (x-w, y-h)

-- ballgenerator :: Float -> [Point3D]
-- ballgenerator radius = (sphere radius)

hacerFoto  :: Float -> Float -> [Point3D] -> [(Int,Int)]
hacerFoto x y puntos3D = map ((floatTupleToIntTuple).(project x y (-30))) puntos3D

ballBase :: [Point3D]
ballBase = (sphere 150)

ballInstance :: [Point3D]
ballInstance =  map (movePoint (0.0, 0.0, 200.0)) ballBase
ballInstance2 :: [Point3D]
ballInstance2 =  map (movePoint ((0), 0, 200.0)) ballBase

foto :: [(Int,Int)]
foto = hacerFoto (-100) (-100) (ballInstance ++ ballInstance2)

-- filledpixels' :: [(Int,Int)]
-- filledpixels' = ballgenerator (-300) (-100) 150

-- filledpixels'' :: [(Int,Int)]
-- filledpixels'' = ballgenerator (-200) (-200) 60

allpixels :: BS.ByteString
allpixels = generateCustomPixelData 400 400 (foto)

main :: IO ()
main = do
      -- print (showDirection (concatDirections (3,3,3) (4,4,4)))
      -- animate window black frame
      -- print ( )
      writeBMP "custom_image.bmp" 400 400 allpixels
      -- mapM_ showPoint3D (sphere 60)
