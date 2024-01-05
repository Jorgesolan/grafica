{-# LANGUAGE BangPatterns #-}
import Escena
import Elem3D
    ( Luz(Luz), 
    RGB(RGB), 
    Direction(Direction), 
    Point3D(Point3D), 
    escalatePoint, escalatePointt,
    degToRad,
    rotatePoint,movePoint, rotatePointt
    )
import Files (writeObject)
import Figuras
    ( Obj,
      Shape(Sphere, Plane, Cylinder,Rectangle, Acelerator),
      Plano(Plano),
      Esfera(Esfera),
      Cilindro(Cilindro),
      Rectangulo(Rectangulo),
      Camara(Camara),
      BVH(BVH),
      buildBVH,
      addFigMult,
      parametricShapeCollision,
      loadObjFile,
      convertToCustomFormat, encenderShape ,encenderShapes
      )
import Funciones (sumFlLuz)
import PathTracer (pathTracer)
import PhotonMap (createPhoton)

import System.Random (StdGen, newStdGen, split)
import System.CPUTime (getCPUTime)

import qualified Data.DList as DL
import qualified Data.Set as Set


main :: IO ()
main = do
  start <- getCPUTime
  gen <- newStdGen
  gen' <- newStdGen

  -- let objFilePath = "../meshes/slab.obj"  
  -- (vertices, triangles) <- loadObjFile objFilePath
  -- let vertices' = map (movePoint (Direction 0 24.9 (-20)).escalatePoint 5) vertices
  -- let customTriangles = convertToCustomFormat (vertices', triangles)
  -- let triangleLus = encenderShapes customTriangles
  --let figuras' = addFigMult triangleLus figuras

  let objFilePath1 = "../meshes/simplepalace.obj"  
  (vertices1, triangles1) <- loadObjFile objFilePath1
  let vertices1' = map (escalatePointt (4).movePoint (Direction 7.5 (-2.5) (-9.75)). rotatePointt 'Y' (282.5)) vertices1
      customTriangles1 = convertToCustomFormat (vertices1', triangles1)
      boundingVol = buildBVH 4000 customTriangles1
      figuras' =  Set.fromList $ addFigMult [(Acelerator boundingVol)] (Set.toList figuras)

  let objFilePath2 = "../meshes/simplehaskell.obj"  
  (vertices2, triangles2) <- loadObjFile objFilePath2
  let vertices2' = map (escalatePointt (1).movePoint (Direction (-5) (-5) (-28)). rotatePointt 'Y' (90)) vertices2
      customTriangles2 = convertToCustomFormat (vertices2', triangles2)
      boundingVol' = buildBVH 4000 customTriangles2
      figuras'' =  Set.fromList $ addFigMult [(Acelerator boundingVol')] (Set.toList figuras')

  let !kdt =  createPhoton (sumFlLuz luces) (DL.fromList []) 0 (round n) figuras'' luces gen' nRebotes
  print $ length kdt
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) :: Float
  putStrLn $ "Tiempo de creacion del kdt: " ++ show diff ++ " segundos"
  writeObject "./kd.bin" kdt
