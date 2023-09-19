import Base
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Geometry.Angle
window :: Display
window = InWindow "random points on sphere with Haskell" (600, 450) (20, 20)

dot :: Color -> Point -> Picture
dot col (x, y) = Color col $ translate x y $ thickCircle 1.0 2.0

greenDot :: Point -> Picture
greenDot (x, y) = dot green (x, y)

sphere :: Float -> [Point3D]
sphere rad = map (\(t, g) -> polarToCartesian t g rad) randomAnglePairs
      where   
            randomAnglePairs = zip thetas gammas
            thetas = take 10000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42
            gammas = take 10000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337


project :: Float -> Float -> Float -> Point3D -> Point
project eyeX eyeY eyeZ (x0, y0, z0) = (projectedX, projectedY)
    where (lookAtX, lookAtY, lookAtZ) = (0.0, 0.0, 0.0)
          (x, y, z) = (x0 - lookAtX, y0 - lookAtY, z0 - lookAtZ)
          (alpha, beta, gamma) = (degToRad 0.0, degToRad 0.0, degToRad 0.0)
          (cosAlpha, sinAlpha) = (cos alpha, sin alpha)
          (cosBeta, sinBeta) = (cos beta, sin beta)
          (cosGamma, sinGamma) = (cos gamma, sin gamma)
          (dx, dy, dz) = (cosBeta*(sinGamma*y + cosGamma*x) - sinBeta*z,
                          sinAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) +
                          cosAlpha*(cosGamma*y - sinGamma*x),
                          cosAlpha*(cosBeta*z + sinBeta*(sinGamma*y + cosGamma*x)) -
                          sinAlpha*(cosGamma*y - sinGamma*x)) 
          projectedX = eyeZ/dz*dx - eyeX
          projectedY = eyeZ/dz*dy - eyeY

frame :: Float -> Picture
frame _ = pictures (map greenDot (imageSpaceCoords ++ imageSpaceCoords2))
      where 
            offset = movePoint (0.0, 0.0, 400.0)
      --     rotation' = rotatePoint 'X' (seconds * 35.0) .
      --               rotatePoint 'Y' (seconds * 45.0)
      --     moved = map (offset . rotation') (sphere 200)
            moved = map (offset) (sphere 60)                       
            imageSpaceCoords = map (project 100 100 (100)) moved
            imageSpaceCoords2 = map (project (-100) (-100) 300) moved

main :: IO ()
main = do
      -- print (showDirection (concatDirections (3,3,3) (4,4,4)))
      animate window black frame

