{-# LANGUAGE RecordWildCards #-}
module KdT where
import Data.KdTree.Static (build, KdTree)
import Elem3D ( Foton(..), Point3D(..) )


{-# INLINE fotonAxis #-}
-- |Función auxiliar, dado un fotón devuelve una lista con sus 3 coordenadas.
fotonAxis :: Foton -> [Float]
fotonAxis (Foton {pFot = Point3D x y z}) = [x, y, z] -- Indicamos simplemente de donde coge las coordenadas paral el kdTree 

{-# INLINE createKD #-}
-- |Función básica, dado una lista de fotones, se crea la estructura KDT correspondiente.
createKD :: [Foton] -> KdTree Float Foton
createKD = Data.KdTree.Static.build fotonAxis -- Creación del kdTree
