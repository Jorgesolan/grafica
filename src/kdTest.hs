import Data.KdTree.Dynamic
-- cabal install kdt
data Foton = Foton !Point3D Float Float

data Point3D = Point3D !Float !Float !Float

instance Show Point3D where
    show (Point3D x y z) = "Point3D " ++ show x ++ " " ++ show y ++ " " ++ show z

instance Show Foton where
    show (Foton (Point3D x y z) i j) = "Foton " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show i ++ " " ++ show j

fotonAxis :: Foton -> [Float]
fotonAxis (Foton (Point3D x y z) _ _) = [x, y, z]

insertMultiple :: KdTree Float Foton -> [Foton] -> KdTree Float Foton
insertMultiple tree [] = tree
insertMultiple tree (p:ps) = insertMultiple (insert tree p) ps

main :: IO ()
main = do
  let dkdt = empty fotonAxis
  let dkdt' = insert dkdt (Foton (Point3D 1.0 1.0 1.0) 5.0 0.0)
  let dkdt'' = insertMultiple dkdt' [(Foton (Point3D 13.0 13.0 13.0) 0.0 3.0),(Foton (Point3D 31.0 31.0 31.0) 7.0 0.0)]
  let a = nearest dkdt'' (Foton (Point3D 25.4 25.4 25.4) 0.0 0.0)
  print a
  let a' = kNearest dkdt'' 2 (Foton (Point3D 22.2 22.0 22.0) 0.0 0.0)
  print a'