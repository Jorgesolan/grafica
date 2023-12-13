module Files where
import Elem3D ( RGB(..) )

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits  ((.&.), shiftR)
import Data.Maybe (isNothing)
import qualified Data.ByteString.Char8 as BS8

import Data.Binary ( Word8, decodeFile, encode, Binary )
import qualified Data.ByteString.Lazy as B

writeObject :: Binary a => FilePath -> a -> IO ()
writeObject path obj = B.writeFile path (encode obj)

readObject :: Binary a => FilePath -> IO a
readObject = decodeFile

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


parsePixels' :: [RGB] -> String
parsePixels' pixels = unwords $ map rgbToString pixels

{-# INLINE rgbToString #-}
rgbToString :: RGB -> String
rgbToString (RGB r g b) = show (round r) ++ " " ++ show (round g) ++ " " ++ show (round b) ++ " "


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
generateBMPPixelData :: Int -> Int -> [(Int, Int)] -> BS.ByteString
generateBMPPixelData width height filledpixels =
    -- BS.pack $ concat $ runEval $ parListChunk 32 rseq $ map generateRow [0 .. height - 1]
    BS.pack $ concatMap generateRow [0 .. height - 1]
  where
    generateRow :: Int -> [Word8]
    generateRow row = concatMap generatePixel [0 .. width - 1]
      where
        generatePixel :: Int -> [Word8]
        generatePixel col
            | (col, row) `elem` filledpixels = [0, 255, 0, 255]
            | otherwise = [0, 0, 0, 255]

-- Function to generate custom pixel data for a checkerboard pattern as a string
generatePPMPixelData :: [Maybe(Float)]  -> String
generatePPMPixelData [] = ""
generatePPMPixelData (x:xs)
  | isNothing x = " 0 0 0" ++ generatePPMPixelData xs
  | otherwise   = " 0 0 255 " ++ generatePPMPixelData xs


-- Function to generate custom pixel data for a checkerboard pattern
fromPixToList :: Int -> Int -> [(Int, Int)] -> String
fromPixToList width height filledpixels =
  -- concat $ runEval $ parListChunk 32 rseq $ map generateRow [0 .. height - 1]
  concatMap generateRow [0 .. height - 1]

  where
    generateRow :: Int -> String
    generateRow row = concatMap generatePixel [0 .. width - 1]
      where
        generatePixel :: Int -> String
        generatePixel col
            | (col, row) `elem` filledpixels = " 0 255 0"
            | otherwise = " 0 0 0"

