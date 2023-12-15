{-# LANGUAGE BangPatterns #-}

module KdTest where
import Data.KdTree.Static ( build, KdTree )

import PhotonMap ()
import Elem3D ( Foton(..), Point3D(Point3D) )
import System.CPUTime (getCPUTime)
import System.Random (StdGen, newStdGen, split)
import Figuras ()
import Funciones ()

--Repetir las veces que nos salga de los webos o hasta que se pierda
{-# INLINE fotonAxis #-}
fotonAxis :: Foton -> [Float]
fotonAxis (Foton (Point3D x y z) _ _ _) = [x, y, z]

-- {-# INLINE insertMultiple #-}
-- insertMultiple :: KdTree Float Foton -> [Foton] -> KdTree Float Foton
-- insertMultiple tree [] = tree
-- insertMultiple tree (p:ps) = insertMultiple (insert tree p) ps

-- createKD :: [Foton] -> KdTree Float Foton
-- createKD fotones = insertMultiple (empty fotonAxis) fotones


createKD :: [Foton] -> KdTree Float Foton
createKD = Data.KdTree.Static.build fotonAxis

-- luz = Luz (Point3D (0) 0 (50)) (RGB 255 255 255) 1
-- luces = [luz]
-- centr = Point3D (-5) (-10) 0
-- centr' = Point3D 13 (-15) (-2)
-- plano0 = Plane (Plano (Point3D (-20) 0 0) (Direction 1 0 0) (RGB 250 255 10) (1, 0, 0) 0) --Izq
-- plano1 =  Plane (Plano (Point3D 20 0 0) (Direction 1 0 0) (RGB 122 10 255) (1, 0, 0) 0) -- Der
-- plano2 =  Plane (Plano (Point3D 0 25 0) (Direction 0 1 0) (RGB 150 150 150) (1, 0, 0) 0) -- Techo
-- plano3 =  Plane (Plano (Point3D 0 0 (-25)) (Direction 0 0 1) (RGB 150 150 150) (1, 0, 0) 0) -- Fondo
-- plano4 =  Plane (Plano (Point3D 0 (-20) 0) (Direction 0 1 0) (RGB 150 150 150) (1, 0, 0) 0) -- Suelo
-- plano5 =  Plane (Plano (Point3D 0 0 (50.5)) (Direction 0 0 1) (RGB 0 0 0) (1, 0, 0) 0) -- Detras Camara
-- bola =  Sphere (Esfera centr 6 (RGB 255 10 10) (1, 0, 0) 0)
-- bola' =  Sphere (Esfera centr' 5 (RGB 10 150 240) (1, 0, 0) 0)
-- bola'' =  Sphere (Esfera centr' 2 (RGB 10 150 240) (0, 0, 0) 0)


-- figuras = addFigMult [bola,bola',plano0,plano1,plano2,plano3, plano4,plano5] [] 
-- n = 1000
-- main :: IO ()
-- main = do
--     start <- getCPUTime
--     gen <- newStdGen

-- --   let dkdt = empty fotonAxis
-- --   let dkdt' = insert dkdt (Foton (Point3D 1.0 1.0 1.0) 5.0 0.0)
-- --   let dkdt'' = insertMultiple dkdt' [(Foton (Point3D 13.0 13.0 13.0) 0.0 3.0),(Foton (Point3D 31.0 31.0 31.0) 7.0 0.0)]
-- --   let a = nearest dkdt'' (Foton (Point3D 25.4 25.4 25.4) 0.0 0.0)
-- --   print a
-- --   let a' = kNearest dkdt'' 2 (Foton (Point3D 22.2 22.0 22.0) 0.0 0.0)
-- --   print a'

--     let  potf = 4*pi*1/n
--     let !a = createPhoton potf [] n figuras luces gen
--     print $ length a
--     mid <- getCPUTime
--     let !b = createKD a
--     end <- getCPUTime
--     let dif = fromIntegral (mid - start) / (10^12) :: Float
--     putStrLn $ "Tiempo de procesar Fotones: " ++ show dif ++ " segundos"
--     let diff = fromIntegral (end - mid) / (10^12) :: Float
--     putStrLn $ "Tiempo de procesado de kdTree: " ++ show diff ++ " segundos"