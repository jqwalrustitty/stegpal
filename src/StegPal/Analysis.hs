{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : StegPal.Analysis
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental

Routines for performing some analysis of the injected images.
-}
-- -----------------------------------------------------------------------------

module StegPal.Analysis
(
-- * Palette Visualisation
  paletteImg
, BlockSize
, checkCandidateDIB
-- * Entropy
, entropy
, entropyBin
, palEntropy
, palEntropyBin
-- * Graphs
, histoGraph
, histoGraphOf
, histoGraphOf'
)
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
--import Data.Binary.Get
import Data.Binary.Put
import Data.Word  (Word8)

import qualified Data.BitVector as BV

import BMP.Bitmap
import ICO.Icons
import StegPal.Histogram
import StegPal.Magic
import StegPal.RGBHist

import Data.List

-- -----------------------------------------------------------------------------

-- | Defines the size in pixels of each colour block.
type BlockSize = Int
type NumCols = Int
type BitCount = Int

baseArray :: Integral a => NumCols -> [[a]]
baseArray colours = reverse $ [ [y..(y+q-1)] |  y <- [0,(q)..(q*(q)-1)] ]
  where
    q = floor $ sqrt $ fromIntegral colours
    --q = ceiling $ sqrt $ fromIntegral colours

resizeLine :: BlockSize -> [a] -> [[a]]
resizeLine blockSize = replicate blockSize . concatMap (replicate blockSize)

resizeArray :: BlockSize -> [[a]] -> [[a]]
resizeArray blockSize = concatMap (replicate blockSize . concatMap (replicate blockSize))

palettePixelArray :: Integral a => BlockSize -> NumCols -> [[a]]
palettePixelArray blockSize numCols = resizeArray blockSize $ baseArray numCols

palettePixels :: BlockSize -> NumCols -> B.ByteString
palettePixels blockSize = B.pack . concat . palettePixelArray blockSize

-- -------------------------------------

palettePixelsPIX :: BitCount -> BlockSize -> B.ByteString
palettePixelsPIX bitCount blockSize = B.pack $ convertPIX $ concat $ palettePixelArray blockSize numCols
  where
    --convertPIX ws = fromPixels $ mapToPix bitCount ws
    convertPIX = fromPixels . mapToPix bitCount
    numCols = 2^bitCount

-- -------------------------------------
{-
We only work with 4-bit and 8-bit palettized images.
-}
checkCandidateDIB :: DIBHeader -> Bool
checkCandidateDIB = flip elem [4,8] . dibBitCount


-- -----------------------------------------------------------------------------
{- |
Create a bitmap representation of the palette.
Useful for visualizing the before and after of an injection.
-}
paletteImg  :: BlockSize    -- ^ size in pixels of each colour block
            -> [RGBQuad]    -- ^ palette
            -> BMP
paletteImg blockSize pal = BMP fh dib pal pxls
  where
    fh  = BMPFileHeader { bfType        = 19778
                        , bfSize        = fromIntegral (fhOffBits + pxlLen)
                        , bfReserved1 = 0, bfReserved2 = 0
                        , bfOffBits     = fromIntegral fhOffBits
                        }
    dib = BMPInfoHeader { dibSize        = fromIntegral bmpInfoHeaderSize
                        , dibWidth       = iWidth
                        , dibHeight      = iHeight
                        , dibPlanes      = 1
                        , dibBitCount    = fromIntegral bitCount
                        , dibCompression = CompressionRGB
                        , dibSizeImage   = fromIntegral pxlLen
                        , dibXPelsPerMeter = 3780, dibYPelsPerMeter = 3780
                        , dibClrUsed = 0, dibClrImportant = 0
                        }
    pxls      = palettePixelsPIX (fromIntegral bitCount) blockSize
    fhOffBits = fromIntegral bmpHeaderSize + fromIntegral bmpInfoHeaderSize + (palLen * 4)
    pxlLen    = fromIntegral $ B.length pxls
    palLen    = length pal
    bitCount  = toInteger $ floor $ (log $ fromIntegral palLen) / (log 2)
    iWidth    = floor $ fromIntegral blockSize * sqrt (fromIntegral palLen)
    iHeight   = iWidth

-- --------------------------------------

paletteImg8 :: BlockSize -> BMP -> BMP
paletteImg8 blockSize bmp = paletteImg blockSize (bmpPalette bmp)

paletteIco8 :: BlockSize -> IconImage -> BMP
paletteIco8 blockSize bmp = paletteImg blockSize (icColors bmp)

-- --------------------------------------


-- -----------------------------------------------------------------------------
{- |
Calculate Shannon entropy of a ByteString
-}
entropy :: (Floating c) => B.ByteString -> c
entropy = entropyCalc . map fromIntegral . filter (/= 0) . vectorCount

entropyCalc :: (Floating c) => [c] -> c
entropyCalc = sum . map lg . fq
  where
    lg :: Floating a => a -> a
    lg c = -c * logBase 2 c
    fq :: Fractional b => [b] -> [b]
    fq c = let sc = sum c in map (/ sc) c

{-
entropyNaive :: (Ord a, Floating c) => [a] -> c
entropyNaive = entropyCalc . map genericLength . group . sort
-}
-- --------------------------------------
{-|
Calculate binary entropy for 'B.ByteString'
-}
entropyBin :: (Floating c) => B.ByteString -> c
entropyBin = entropyCalc . map fromIntegral . filter (/= 0) . vectorCountN 2 . toBin
  where
    toBin = concatMap (map fromEnum . BV.toBits) . BV.bitVecs 8 . B.unpack

-- --------------------------------------

palEntropy :: (Floating c) => Image -> c
palEntropy (ImgICO i) = entropy $ B.concat $ map (runPut.put.histPalette) $ iconImgs i
palEntropy (ImgBMP i) = entropy (runPut $ put $ histPalette i)
palEntropy _ = 0

palEntropyBin :: (Floating c) => Image -> c
palEntropyBin (ImgICO i) = entropyBin $ B.concat $ map (runPut.put.histPalette) $ iconImgs i
palEntropyBin (ImgBMP i) = entropyBin (runPut $ put $ histPalette i)
palEntropyBin _ = 0


-- =============================================================================



{- |
...
-}
histoGraphOf :: HistInjectable a => a -> [String]

histoGraphOf i = reverse $ transpose $ map (padSpace (z+1)) q
  where
    h = histogram i
    s = fromIntegral $ sum h
    z = maximum $ map length q
    q = map (\x -> replicate (ceiling $ 100 * fromIntegral x/s) '*') h


histoGraphOf' i = q ++ b
  where
    q = histoGraphOf i
    b = [bottomLine (maximum $ map length q)]

-- --------------------------------------
{- |
...
-}
histoGraph :: Image -> [String]

histoGraph (ImgICO i) = concatMap (histoGraphOf') $ iconImgs i

histoGraph (ImgBMP i) = histoGraphOf' i

histoGraph _ = undefined


-- -------------------------------------

padSpace :: Int -> String -> String
padSpace n xs = xs ++ (replicate (n - length xs) ' ')


bottomLine n = take n $ concat $ repeat ("|" ++ replicate 9 '-')


-- -------------------------------------

--histoGraph :: Image -> [String]
--histoGraph :: HistInjectable a => a -> [String]

{-
--histoGraph (ImgBMP i) = reverse $ transpose $ map (padSpace (z+1)) q
histoGraph (ImgBMP i) = reverse $ transpose $ map (padSpace (z+1)) q
  where
    h = histogram i
    s = fromIntegral $ sum h
    z = maximum $ map length q
    q = map (\x -> replicate (ceiling $ 100 * fromIntegral x/s) '*') h
    --r = map (\x -> ceiling $ 100 * fromIntegral x/s) h
    --q = map (\n -> replicate n '*') r
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
