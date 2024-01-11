{-# LANGUAGE RecordWildCards #-}
module Files where
import Elem3D ( RGB(..) )

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits  ((.&.), shiftR)
import Data.Maybe (isNothing)
import qualified Data.ByteString.Char8 as BS8

import Data.Binary ( Word8, decodeFile, encode, Binary )
import qualified Data.ByteString.Lazy as B
import qualified Data.DList as DL

-- |Función básica, dado un objeto que instancie la clase Binary, escribe este en formato binario a un archivo en disco.
writeObject :: Binary a => FilePath -> DL.DList a -> IO ()
writeObject path obj = B.writeFile path (encode (DL.toList obj))

-- |Función básica, dado un fichero binario, recupera el objeto que haya almacenado en este y lo devuelve.
readObject :: Binary a => FilePath -> IO a
readObject = decodeFile

-- |Función auxiliar, para leer un archivo .ppm y almacenar los píxeles en una lista.
leerPPM :: FilePath -> IO ([RGB], (Float, Float, Float, Float))
leerPPM archivo = do
    contenido <- BS8.readFile archivo
    let lineas = BS8.lines contenido
        ppMax = findMaxPPM lineas
        sizeLine = findSizePPM lineas
        valueMax = read . BS8.unpack $ lineas !! 4
        pixelLines = drop 5 lineas
        pixelesParseados = concatMap (parsePixels . BS8.words) pixelLines
    return (pixelesParseados, (fst sizeLine, snd sizeLine, valueMax, ppMax))

-- |Función auxiliar, para extraer el MAX dentro de un ppm.
findMaxPPM :: [BS8.ByteString] -> Float
findMaxPPM [] = 0.0
findMaxPPM (linea:resto)
    | BS8.isPrefixOf (BS8.pack "#MAX=") linea = read (BS8.unpack $ BS8.drop 5 linea)
    | otherwise = findMaxPPM resto

-- |Función auxiliar, extrae el tamaño de un ppm.
findSizePPM :: [BS8.ByteString] -> (Float, Float)
findSizePPM [] = (0,0)
findSizePPM (linea:resto)
    | BS8.isPrefixOf (BS8.pack "#") linea = findSizePPM resto
    | otherwise = case BS8.words linea of
        [num1, num2] -> (read $ BS8.unpack num1, read $ BS8.unpack num2)
        _ -> findSizePPM resto


-- |Función auxiliar, para analizar una línea de píxeles y convertirla en una lista de RGB.
parsePixels :: [BS8.ByteString] -> [RGB]
parsePixels [] = []
parsePixels (r:g:b:resto) =
    let rgb = RGB (read $ BS8.unpack r) (read $ BS8.unpack g) (read $ BS8.unpack b)
    in rgb : parsePixels resto
parsePixels _ = error "Formato incorrecto"

-- |Función auxiliar, convierte una lista de RGBs en un string de pixeles.
parsePixels' :: [RGB] -> String
parsePixels' pixels = unwords $ map rgbToString pixels

{-# INLINE rgbToString #-}
-- |Función auxiliar, convierte un RGB a string.
rgbToString :: RGB -> String
rgbToString (RGB {..}) = show (round $ red * 255) ++ " " ++ show (round$  green * 255) ++ " " ++ show (round $ blue * 255) ++ " "


-- |Función auxiliar, escribe un 32-bit BMP file
writeBMP :: FilePath -> Int -> Int -> BS.ByteString -> IO ()
writeBMP filename width height customPixelData = do
    let fileSize = 54 + 4 * width * height
    let pixelDataOffset = 54
    let dibHeaderSize = 40
    let bitsPerPixel = 32
    let compressionMethod = 0
    let imageSize = 4 * width * height
    let xPixelsPerMeter = 2835 -- 72 DPI
    let yPixelsPerMeter = 2835 -- 72 DPI

    BS.writeFile filename $ BS.concat
        [ BS.pack [66, 77]                      -- Signature ("BM")
        , intTo4Bytes fileSize                 -- File size
        , intTo4Bytes 0                        -- Reserved
        , intTo4Bytes pixelDataOffset           -- Pixel data offset
        , intTo4Bytes dibHeaderSize             -- DIB header size
        , intTo4Bytes width                    -- Image width
        , intTo4Bytes height                   -- Image height
        , intTo2Bytes 1                        -- Number of color planes
        , intTo2Bytes bitsPerPixel             -- Bits per pixel
        , intTo4Bytes compressionMethod        -- Compression method
        , intTo4Bytes imageSize                -- Image size
        , intTo4Bytes xPixelsPerMeter          -- Horizontal resolution (pixels per meter)
        , intTo4Bytes yPixelsPerMeter          -- Vertical resolution (pixels per meter)
        , BS.replicate 8 0                     -- Reserved
        , customPixelData  -- White pixel data
        ]

-- |Función principal, escibre en un archivo PPM la información de los pixels en formato P3.
writePPM :: FilePath -> Int -> Int -> String -> IO ()
writePPM filename width height customPixelData = do
    let maxColorValue = 255
    let header = unlines
            [ "P3"
            , "#MAX=255"
            , "# " ++ filename
            , show width ++ " " ++ show height
            , show maxColorValue
            ]
    BS8.writeFile filename $ BS8.unlines
        [ BS8.pack header
        , BS8.pack customPixelData
        ]

-- |Función auxiliar, convierte un entero a un ByteString de 4 Bytes.
intTo4Bytes :: Int -> BS.ByteString
intTo4Bytes n = BS.pack [fromIntegral (n .&. 0xFF), fromIntegral ((n `shiftR` 8) .&. 0xFF), fromIntegral ((n `shiftR` 16) .&. 0xFF), fromIntegral ((n `shiftR` 24) .&. 0xFF)]

-- |Función auxiliar, convierte un entero a un ByteString de 2 Bytes.
intTo2Bytes :: Int -> BS.ByteString
intTo2Bytes n = BS.pack [fromIntegral (n .&. 0xFF), fromIntegral ((n `shiftR` 8) .&. 0xFF)]

-- |Función auxiliar, convierte una lista ordenada de RGBs a BytseString.
pixels2BMP :: [RGB] -> BS.ByteString
pixels2BMP rgbList = BS.pack $ concatMap rgbToWord8 $ reverse rgbList
  where
    -- |Función auxiliar, convierte un RGB a una lista de Word8.
    rgbToWord8 :: RGB -> [Word8]
    rgbToWord8 (RGB r g b) = map (fromIntegral.round) [r, g, b, 255]