import Base
import Graphics.Gloss
import System.Random
import Graphics.Gloss.Geometry.Angle
window :: Display
window = InWindow "random points on sphere with Haskell" (600, 450) (20, 20)

dot :: Color -> Point -> Picture
dot col (x, y) = Color col $ translate x y $ thickCircle 1.0 2.0

redDot :: Point -> Picture
redDot (x, y) = dot red (x, y)

sphere :: Float -> [Point3D]
sphere rad = map (\(t, g) -> polarToCartesian t g rad) randomAnglePairs
    where randomAnglePairs = zip thetas gammas
          thetas = take 2000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42
          gammas = take 2000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337


sphereInstance :: [Point3D]
sphereInstance = sphere 150

project :: Float -> Float -> Float -> Point3D -> Point
project eyeX eyeY eyeZ (x0, y0, z0) = (projectedX, projectedY)
    where (lookAtX, lookAtY, lookAtZ) = (0.0, 0.0, 0.0)
          (x, y, z) = (x0 - lookAtX, y0 - lookAtY, z0 - lookAtZ)
          (alpha, beta, gamma) = (degToRad 0.0, degToRad 0.0, degToRad 0.0)
      --     (eyeX, eyeY, eyeZ) = (0.0, 0.0, 300.0)
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

projectInstance :: Point3D -> Point
projectInstance = project 100 20 200

frame :: Float -> Picture
frame seconds = pictures (map redDot imageSpaceCoords)
                where offset = movePoint (0.0, 0.0, 400.0)
                      rotation' = rotatePoint 'X' (seconds * 35.0) .
                                rotatePoint 'Y' (seconds * 45.0)
                      moved = map (offset . rotation') sphereInstance
                      imageSpaceCoords = map projectInstance moved

main :: IO ()
main = do
      print (showDirection (concatDirections (3,3,3) (4,4,4)))
      animate window white frame

