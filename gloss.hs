-- import Graphics.Gloss
-- import Graphics.Gloss.Geometry.Angle
-- window :: Display
-- window = InWindow "random points on sphere with Haskell" (600, 450) (20, 20)

-- dot :: Color -> Point -> Picture
-- dot col (x, y) = Color col $ translate x y $ thickCircle 1.0 2.0

-- greenDot :: Point -> Picture
-- greenDot (x, y) = dot green (x, y)

-- fatSphere :: Color -> Float -> Point -> Picture
-- fatSphere col radius (x, y) = Color col $ translate x y $ thickCircle 1.0 radius

-- frame :: Float -> Picture
-- frame seconds = pictures ((map (fatSphere blue 150) (imageSpaceCoords ++ imageSpaceCoords2)) ++ (map greenDot (imageSpaceCoords' ++ imageSpaceCoords2')))
--       where 
--             offset = movePoint (0.0, 0.0, 400.0)
--             rotation' = rotatePoint 'X' (seconds * 35.0) .
--                     rotatePoint 'Y' (seconds * 45.0)
--             -- moved = map (offset . rotation') (sphere 200)
--             moved = map (offset . rotation') [(0.0, 1.0, (-2.0))] 
--             moved' = map (offset . rotation') (sphere 150)                       
--             imageSpaceCoords = map (project 100 100 (1000)) moved
--             imageSpaceCoords2 = map (project (-100) (-100) (-10)) moved
--             imageSpaceCoords' = map (project 100 100 (1000)) moved'
--             imageSpaceCoords2' = map (project (-100) (-100) (-10)) moved'