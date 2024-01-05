{-# LANGUAGE RecordWildCards #-}
module KdTest where
import Data.KdTree.Static (build, KdTree)
import Elem3D ( Foton(..), Point3D(..) )


{-# INLINE fotonAxis #-}
fotonAxis :: Foton -> [Float]
fotonAxis (Foton {pFot = Point3D x y z}) = [x, y, z] -- Indicamos simplemente de donde coge las coordenadas paral el kdTree 

{-# INLINE createKD #-}
createKD :: [Foton] -> KdTree Float Foton
createKD = Data.KdTree.Static.build fotonAxis -- Creación del kdTree
