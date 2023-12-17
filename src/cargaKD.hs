import Escena
import Elem3D
    ( Luz(Luz), 
    RGB(RGB), 
    Direction(Direction), 
    Point3D(Point3D), 
    escalatePoint, 
    degToRad,
    rotatePoint,movePoint
    )
import Files (writePPM, rgbToString,writeObject)
import Tone_map (gammaFunc,clamp)
import Figuras
    ( Obj,
      Shape(Sphere, Plane, Cylinder,Rectangle),
      Plano(Plano),
      Esfera(Esfera),
      Cilindro(Cilindro),
      Rectangulo(Rectangulo),
      Camara(Camara),
      addFigMult,
      parametricShapeCollision,
      loadObjFile,
      convertToCustomFormat, encenderShape ,encenderShapes
      )
import Funciones (sumFlLuz)
import PathTracer (pathTracer)
import KdTest ()
import PhotonMap ( createPhoton )
import Data.KdTree.Static ()

import System.Random (StdGen, newStdGen, split)
import Data.List (transpose)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)

-- make clean && make sim && ./sim -N12 && convert a.ppm out.bmp
-- make clean && make sim && ./sim  +RTS -N -l -RTS && convert a.ppm out.bmp
-- make clean && make simulacion && cd ./tmp && ./run.sh && cd .. && convert ./tmp/output.ppm a.bmp


-- Hacer funcion para a un nuevo objeto, meterle el RGB


main :: IO ()
main = do
  start <- getCPUTime
  gen <- newStdGen
  gen' <- newStdGen

  let objFilePath = "../meshes/slab.obj"  
  (vertices, triangles) <- loadObjFile objFilePath
  let vertices' = map (movePoint (Direction 0 24.9 (-20)).escalatePoint 5) vertices
  let customTriangles = convertToCustomFormat (vertices', triangles)
  let triangleLus = encenderShapes customTriangles
  --let figuras' = addFigMult triangleLus figuras

  let kdt =  createPhoton (sumFlLuz luces) [] 0 (round n) figuras luces gen' nRebotes
  print $ length kdt
  writeObject "./kd.bin" kdt
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) :: Float
  putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"
