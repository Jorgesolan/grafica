module Files where
import Elem3D
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits  ((.&.), shiftR)
import qualified Data.ByteString.Char8 as BS8
import System.IO

-- Función para leer un archivo .ppm y almacenar los píxeles en una lista
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

findMaxPPM :: [BS8.ByteString] -> Float
findMaxPPM [] = 0.0
findMaxPPM (linea:resto)
    | BS8.isPrefixOf (BS8.pack "#MAX=") linea = read (BS8.unpack $ BS8.drop 5 linea)
    | otherwise = findMaxPPM resto

findSizePPM :: [BS8.ByteString] -> (Float, Float)
findSizePPM [] = (0,0)
findSizePPM (linea:resto)
    | BS8.isPrefixOf (BS8.pack "#") linea = findSizePPM resto
    | otherwise = case BS8.words linea of
        [num1, num2] -> (read $ BS8.unpack num1, read $ BS8.unpack num2)
        _ -> findSizePPM resto


-- Función para analizar una línea de píxeles y convertirla en una lista de RGB
parsePixels :: [BS8.ByteString] -> [RGB]
parsePixels [] = []
parsePixels (r:g:b:resto) =
    let rgb = RGB (read $ BS8.unpack r) (read $ BS8.unpack g) (read $ BS8.unpack b)
    in rgb : parsePixels resto
parsePixels _ = error "Formato incorrecto"

singlepixelReesclate :: Float -> RGB -> RGB
singlepixelReesclate x (RGB a b c) = RGB (a * x ) (b * x) (c * x) 

pixelReesclate :: Float -> [RGB] -> [RGB]
pixelReesclate x puntosRGB = puntosRGBEcualizados where
    puntosRGBEcualizados = map (singlepixelReesclate x) puntosRGB

-- Define a helper function for recursive parsing
parsePixelsRecursive :: Int -> Int -> [RGB] -> [Char] -> [Char]
parsePixelsRecursive _ _ [] acc = acc -- Base case: empty list, return the accumulator
parsePixelsRecursive counter x (RGB r g b : rest) acc
  | counter == 0 = parsePixelsRecursive x x rest (acc ++ "\n" ++ show (round r) ++ " " ++ show (round g) ++ " " ++ show (round b) ++ " ")
  | otherwise = parsePixelsRecursive (counter - 1) x rest (acc ++ show (round r) ++ " " ++ show (round g) ++ " " ++ show (round b) ++ " ")
  
-- Main parsePixels function
parsePixels' :: Int -> [RGB] -> [Char]
parsePixels' x pixels = parsePixelsRecursive x x pixels ""

parsePixels'' :: [RGB] -> [String]
parsePixels'' pixels = map rgbToString pixels
  where
    rgbToString (RGB r g b) = show (round r) ++ " " ++ show (round g) ++ " " ++ show (round b)


-- Function to write a 32-bit BMP file
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

-- Function to write a PPM file with custom pixel data in P3 format
writePPM :: FilePath -> Int -> Int -> [String] -> IO ()
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
        ] <> BS8.unlines (map BS8.pack customPixelData)  -- Pixel data

-- Helper function to convert an Int to a ByteString of 4 bytes
intTo4Bytes :: Int -> BS.ByteString
intTo4Bytes n = BS.pack [fromIntegral (n .&. 0xFF), fromIntegral ((n `shiftR` 8) .&. 0xFF), fromIntegral ((n `shiftR` 16) .&. 0xFF), fromIntegral ((n `shiftR` 24) .&. 0xFF)]

-- Helper function to convert an Int to a ByteString of 2 bytes
intTo2Bytes :: Int -> BS.ByteString
intTo2Bytes n = BS.pack [fromIntegral (n .&. 0xFF), fromIntegral ((n `shiftR` 8) .&. 0xFF)]

-- Function to convert ordered RGB list to ByteString
pixels2BMP :: [RGB] -> BS.ByteString
pixels2BMP rgbList = BS.pack $ concatMap rgbToWord8 $ reverse rgbList
  where
    -- Helper function to convert RGB to Word8 list
    rgbToWord8 :: RGB -> [Word8]
    rgbToWord8 (RGB r g b) = map (fromIntegral.round) [r, g, b, 255]

-- Function to generate custom pixel data for a checkerboard pattern
generateBMPPixelData :: Int -> Int -> [(Int,Int)] -> BS.ByteString
generateBMPPixelData width height filledpixels =
    BS.pack $ concatMap generateRow [0 .. height - 1]
  where
    generateRow :: Int -> [Word8]
    generateRow row = concatMap generatePixel [0 .. width - 1]
      where
        generatePixel :: Int -> [Word8]
        generatePixel col
            | (col, row) `elem` filledpixels = [0, 255, 0, 255]
            | otherwise = [0,0,0,255]

-- Function to generate custom pixel data for a checkerboard pattern as a string
generatePPMPixelData :: Int -> Int -> [(Int, Int)] -> String
generatePPMPixelData width height filledpixels =
    unlines $ map generateRow [0 .. height - 1]
  where
    generateRow :: Int -> String
    generateRow row = unwords $ map generatePixel [0 .. width - 1]
      where
        generatePixel :: Int -> String
        generatePixel col
            | (col, row) `elem` filledpixels = "0 255 0"
            | otherwise = "0 0 0"

--main :: IO ()
--main = do
--   let width = 20  -- Width of the image
--       height = 20 -- Height of the image
--   let customPixelData = generateBMPPixelData width height [(1,10),(2,10),(3,10),(10,10),(5,10),(6,10)] -- Replace this with your custom pixel data
--   let customPixelData2 = generatePPMPixelData width height [(1,10),(2,10),(3,10),(10,10),(5,10),(6,10)] -- Replace this with your custom pixel data
--
--   writeBMP "custom_image.bmp" width height customPixelData
--   writePPM "output.ppm" width height customPixelData2