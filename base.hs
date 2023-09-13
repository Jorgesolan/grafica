type Direction = (Float, Float, Float)

type Point =  (Float, Float, Float) 

-- Resta de puntos -> Dirección del primero al segundo
(##) :: Point -> Point -> Direction
(xb, yb, zb) ## (xa, ya, za)  = (xb-xa, yb-ya, zb-za) 

-- desplazamiento de un punto
desplazarPunto :: Point -> Direction -> Direction
desplazarPunto (xb, yb, zb) (xa, ya, za)  = (xb+xa, yb+ya, zb+za) 


-- Suma de direcciones
(+++) :: Direction -> Direction -> Direction
(xb, yb, zb) +++ (xa, ya, za)  = (xb+xa, yb+ya, zb+za) 

-- Producto escalar
(...) :: Direction -> Direction -> Float
(xb, yb, zb) ... (xa, ya, za)  = (xb*xa + yb*ya + zb*za) 

-- Producto vectorial
(***) :: Direction -> Direction -> Float
(xa, ya, za) *** (xb, yb, zb) = 0
-- (xa, ya, za) *** (xb, yb, zb) = ((ya*zb-za*yb),(za*xb-xa*zb),(xa*yb-ya*xb))

-- Escalado de dirección
escalarDir :: Float -> Direction -> Direction
escalarDir s (xa, ya, za) = (s*xa, s*ya, s*za)

-- Modulo
modd :: Direction -> Float
modd (xb, yb, zb) = sqrt(xb^2 + yb^2 + zb^2)

--Normalizacion
norm :: Direction -> Direction
norm b = (xb / bmod, yb / bmod, zb / bmod) where 
    (xb, yb, zb) = b
    bmod = modd b
    
a :: Point
a = (3, 3, 3)
b :: Point
b = (5, 5, 5)
res = b ## a


c :: Direction
c = (1, 1, 1)
d :: Direction
d = (3, 3, 3)
res2 = c +++ d
-- res3 = 5 *** c
res4 = c ... d

res5 = desplazarPunto a c
main = do
    print res
    print res2
    -- print res3
    print res4
    print res5