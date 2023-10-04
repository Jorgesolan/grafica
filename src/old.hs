-- import System.Random
-- sphere :: Float -> [Point3D]
-- sphere rad = map (\(t, g) -> polarToCartesian t g rad) randomAnglePairs
--       where   
--             -- randomAnglePairs = zip thetas gammas
--             -- thetas = take 1000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 42
--             -- gammas = take 1000 $ randomRs (0.0::Float, 360.0) $ mkStdGen 1337
--             list1 = [0.0,360.0..360.0]
--             list2 = [0.0,360.0..360.0]
--             randomAnglePairs = combinate2Tuples list1 list2                        



-- parametricSphereInstance = parametricSphereCollision (Point3D 0 0 10) 3



-- cube :: Float -> [Point3D]
-- cube size = cubePoints
--       where   
--             list1 = [0.0,2.0..size]
--             list2 = [0.0,2.0..size]
--             list3 = [0.0,2.0..size]
--             cubePoints = combinate3Tuples list1 list2 list3                     



-- -- ballgenerator :: Float -> [Point3D]
-- -- ballgenerator radius = (sphere radius)


-- ballBase :: [Point3D]
-- ballBase = (sphere 10)

-- cubeBase :: [Point3D]
-- cubeBase = (cube 50)

-- ballInstance :: [Point3D]
-- ballInstance =  map (movePoint (0.0, 300.0, 300.0)) ballBase

-- cubeInstance :: [Point3D]
-- cubeInstance =  map (movePoint ((-15), (-15), 100.0) . rotatePoint 'Y' 20.0 . rotatePoint 'X' 45.0 ) cubeBase

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