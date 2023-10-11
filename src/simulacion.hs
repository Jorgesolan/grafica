import Elem3D
import Files
import Figuras
import Data.List
import Data.Maybe
import Control.Parallel.Strategies (using, rseq, parListChunk)
import Control.DeepSeq (force)
import Debug.Trace
import Data.List (any)
import System.CPUTime
-- make clean && make sim && ./sim -N && convert a.ppm out.bmp

generateRaysForPixels :: Camara -> Float -> Float -> [Ray]
generateRaysForPixels (Camara p (Base (Direction px _ _) (Direction _ py _) (Direction _ _ focal))) width height =
  [Ray p (generateDirection x y focal) 10 | y <- [(-py'), (piY-py') ..(py'-piY)], x <- [(-px'), (piX-px') ..(px'-piX)]]
  where
      piY = py / width
      piX = px / height
      px' = px / 2
      py' = py / 2
      generateDirection :: Float -> Float -> Float -> Direction
      generateDirection width height focal = normal ((Point3D width height focal) #< p)

obtenerPrimeraColision :: [(Float, (RGB,Float, Point3D, Direction))] -> (Float,(RGB,Float, Point3D, Direction))
obtenerPrimeraColision = foldl1' (\acc@(minFloat,( _,_, _, _)) (x, (rgb, ref,collisionPoint, normal)) -> if (x < minFloat && x >= 0) then (x, (rgb, ref,collisionPoint, normal)) else acc)

listRayToRGB ::  Point3D -> Point3D -> ([Ray] -> [[(Float,(RGB,Float,Point3D,Direction))]]) -> [[(Float, (RGB,Float, Point3D, Direction))]] -> [RGB]
listRayToRGB luz cam figuras listaDeListas = trace (show (length listaDeListas)) $ map (calcularColor.snd.obtenerPrimeraColision) (transpose listaDeListas)
  where
    calcularColor (rgb,ref,collisionPoint,normal) = newRgb  -- Return a default value when there's no collision
      where
        -- calcular rayo de punto de colisión a luz, su distancia y angulo
        ligthDir = (luz #< collisionPoint)
        ligthRay =  Ray collisionPoint ligthDir dluz
        dluz = modd ligthDir        
        angle = (normal .* ligthDir) / ((modd normal) * (modd ligthDir))
        -- lanzarlo y quedarse con las colisiones de todos los objetos, comprobar si hay alguna colisión a menor distancia que la esperada
        -- en la siguiente linea falta un transpose con toda seguridad!!!!!!!!!!!!!!!!
        collisions:_ = (figuras [ligthRay])
        primerImpactoLuz = fst (obtenerPrimeraColision collisions)
        colisiona = primerImpactoLuz < (dluz)

        -- calcular angulo de reflexión (esta mal ahora mismo es el ángulo a la camara por eso el espejo esta del reves)
        reflectedDir = (cam #< collisionPoint)
        -- quedarse con el color del primer impacto del rayo reflectado
        (reflectedPointRGB, _, _, _) =  (snd.obtenerPrimeraColision.head) (transpose (figuras [(Ray collisionPoint reflectedDir 2)]))
        reflectedRGB = agregateRGBPoints (rgbProd rgb (1 - ref)) (rgbProd reflectedPointRGB ref)
        newRgb = if not colisiona then rgbProd reflectedRGB (1 - (angle / 90.0)) else rgbProd reflectedRGB (1/3)


pix :: Float
pix = 1400
piCam :: Float
piCam = 250
basCam = Base (Direction piCam 0 0) (Direction 0 piCam 0) (Direction 0 0 (-500))
centr = Point3D (0) (0) 0
centr' = Point3D (-50) 200 0
centr'' = Point3D (100) (200) (-20)
triangulo = Triangulo (Point3D (5) (25) 70) (Point3D (15) (5) 70) (Point3D (5) (5) 70) (RGB 255 0 255)
luz = Point3D (100) (-175) (-20)
cam' =  Point3D (0) (0) (-1000)
plano0 = Plane (Plano (Point3D (-200) 0 200) (Direction 1 0 0) (RGB 249 176 84) 0)
plano1 =  Plane (Plano (Point3D (200) 0 200) (Direction (1) (0) (0)) (RGB 146 223 222) 0)
plano2 =  Plane (Plano (Point3D 0 (200) 200) (Direction 0 (-1) 0) (RGB 0 255 0) 0)
plano3 =  Plane (Plano (Point3D 0 0 200) (Direction 0 0 (-1)) (RGB 175 170 169) 0)
plano4 =  Plane (Plano (Point3D 0 (-250) 200) (Direction 0 (-1) (0)) (RGB 255 0 255) 0)
bola =  Sphere (Esfera centr 50 (RGB 255 0 0) 0)
bola' =  Sphere (Esfera centr' 40 (RGB 0 0 255) 0)
bola'' =  Sphere (Esfera centr'' 50 (RGB 155 0 155) 0.95)
bolaLus = Sphere (Esfera luz 10 (RGB 255 255 255) 0)
camara = Camara (Point3D (0) (0) (-1000)) basCam

-- Función para extraer los puntos de las tuplas
obtenerPuntos :: [(Float, (RGB,Float, Point3D, Direction))] -> [Point3D]
obtenerPuntos lista = map (\(_, (_,_, point, _)) -> point) lista

figuras = (parametricShapeCollision [bola,bola',bola'',plano0,plano1,plano2,plano3,plano4])
main :: IO ()
main = do

      start <- getCPUTime

      let rayitos = generateRaysForPixels camara pix pix `using` parListChunk 32 rseq
      let sol = force figuras rayitos `using` parListChunk 32 rseq
      let a = concat $ map rgbToString . (listRayToRGB luz cam' (figuras)) $ sol
      -- let solBolaLus =  force parametricShapeCollision [bolaLus] rayitos
      -- let solo = listRay sol
      -- let lusesita = force map (\punto -> Ray luz (punto #< luz) 0) $ obtenerPuntos solo 
      -- let solLus = force parametricShapeCollision [bola,bola',plano0,plano1,plano2,plano3,plano4] lusesita `using` parListChunk 32 rseq
      -- let sol' = luzXRayo (solo) (listRay solLus)
      -- let sol'' = listRay [sol',(concat solBolaLus)]
      -- let a = map rgbToString $ map (\(_,(rgb,_,_,_)) -> rgb) sol''
      -- writePPM "a.ppm" (round pix) (round pix) (concat a)
      
      writePPM "a.ppm" (round pix) (round pix) a
      
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12) :: Double
      putStrLn $ "Tiempo de procesado de la imagen: " ++ show diff ++ " segundos"
