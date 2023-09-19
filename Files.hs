module Files where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits  ((.&.), shiftR)
-- Function to write a 32-bit BMP file with a white background
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

-- Helper function to convert an Int to a ByteString of 4 bytes
intTo4Bytes :: Int -> BS.ByteString
intTo4Bytes n = BS.pack [fromIntegral (n .&. 0xFF), fromIntegral ((n `shiftR` 8) .&. 0xFF), fromIntegral ((n `shiftR` 16) .&. 0xFF), fromIntegral ((n `shiftR` 24) .&. 0xFF)]

-- Helper function to convert an Int to a ByteString of 2 bytes
intTo2Bytes :: Int -> BS.ByteString
intTo2Bytes n = BS.pack [fromIntegral (n .&. 0xFF), fromIntegral ((n `shiftR` 8) .&. 0xFF)]

-- Function to generate custom pixel data for a checkerboard pattern
generateCustomPixelData :: Int -> Int -> [(Int,Int)] -> BS.ByteString
generateCustomPixelData width height filledpixels =
    BS.pack $ concatMap generateRow [0 .. height - 1]
  where
    generateRow :: Int -> [Word8]
    generateRow row = concatMap generatePixel [0 .. width - 1]
      where
        generatePixel :: Int -> [Word8]
        generatePixel col
            | (row, col) `elem` filledpixels = [0, 255, 0, 255]
            | otherwise = [0,0,0,255]
-- main :: IO ()
-- main = do
--     let width = 800  -- Width of the image
--         height = 600 -- Height of the image
--     let customPixelData = generateCustomPixelData width height  -- Replace this with your custom pixel data
    
--     writeBMP "custom_image.bmp" width height customPixelData