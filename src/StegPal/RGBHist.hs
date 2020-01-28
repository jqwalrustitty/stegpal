{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : StegPal.RGBHist
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental

Module for injecting into the unused parts of the palette


=== Injection

1.  Take a histogram of the pixels per palette entry.

2.  Where the entry is zero, the palette entry is unused, so inject bytes
    into the 'RGBQuad' parameters of that entry.  (Leaves 'rgbReserved' untouched.)
    Use an artificial /null/ padding to mark end of inserted data.

=== Extraction

1.  Take a histogram of the pixels per palette entry.

2.  Where the histogram count is zero, read those bytes until we encounter
    a /null/ entry.

-}
-- -----------------------------------------------------------------------------

module StegPal.RGBHist
(
  HistInjectable(..)
)
where

import qualified Data.ByteString.Lazy as B
import GHC.Int (Int64)

import BMP.Bitmap
import ICO.Icons

--import StegPal.Encoder
import StegPal.Histogram
import StegPal.Magic


-- -----------------------------------------------------------------------------
-- * HistInjectable class

{- |
Class that defines all of the actions for injecting into
an image.

Basically, 'histPalette', 'histBits' and 'histPixels' are accessors into
the data type, which allows most of the other methods to be automatically
derived.
-}
class HistInjectable image where
  -- | Image palette accessor
  histPalette :: image -> [RGBQuad]

  -- | Bit Count for image.  Essentially log_2 of the number of colours.
  histBits    :: Integral n => image -> n

  -- | Image pixel accessor
  histPixels  :: image -> B.ByteString

  -- | Creates the histogram of the pixels per palette entry.
  histogram   :: image -> [Int]
  histogram img = vectorCountSafe n pxls
    where 
      pxls  = toPixelsRaw n $ B.unpack $ histPixels img
      n     = histBits img

  -- | How many bytes can we inject into the palette.
  histSpace   :: image -> Int
  histSpace = (*3) . length . filter (==0) . histogram

  -- | Injects 'ByteString' into the image, and returns the new image.
  histInject  :: image -> B.ByteString -> image

  -- | Extracts the data from the palette of the image.
  histExtract :: image -> B.ByteString
  histExtract img = palExtractor (histPalette img) (histogram img)

-- -------------------------------------

-- Creates a new palette from the original image and the injected data.
histNewPalette :: (HistInjectable image) => image -> B.ByteString -> [RGBQuad]
histNewPalette img bs = palInjector (histPalette img) (histogram img) (chunkB 3 bs)

-- -------------------------------------

instance HistInjectable BMP where
  histPalette = bmpPalette
  histBits    = fromIntegral . dibBitCount . bmpDIBHeader
  histPixels  = bmpPixels

  histInject img bs = img {bmpPalette = (histNewPalette img bs) }

-- -------------------------------------

instance HistInjectable IconImage where
  histPalette = icColors
  histBits    = fromIntegral . dibBitCount . icHeader
  histPixels  = icXOR

  histInject img bs = img { icColors = (histNewPalette img bs) }

-- -------------------------------------
{- |
This doesn't define the 'histPalette', 'histBits' or 'histPixels' parameters,
instead acts as a sort of wrapper around 'IconImage'.
-}
instance HistInjectable Icon where
  histPalette = undefined
  histBits    = undefined
  histPixels  = undefined

  --histogram   = undefined
  histogram   = concatMap histogram . iconImgs      -- this is wrong, but needed for histSpace

  histExtract = B.concat . map histExtract . iconImgs
  histInject img bs = img { iconImgs = newImgs }
    where
      newImgs = zipWith histInject (iconImgs img) (partPack img bs)

-- -------------------------------------
{-
Wrapper for 'Image'.
-}
instance HistInjectable Image where
  histPalette (ImgBMP a) = histPalette a
  histPalette (ImgICO a) = histPalette a

  histBits (ImgBMP a)    = histBits a
  histBits (ImgICO a)    = histBits a

  histPixels (ImgBMP a)  = histPixels a
  histPixels (ImgICO a)  = histPixels a

  histogram (ImgBMP a)   = histogram a
  histogram (ImgICO a)   = histogram a

  histExtract (ImgBMP a) = histExtract a
  histExtract (ImgICO a) = histExtract a

  histInject (ImgBMP a) = ImgBMP . histInject a
  histInject (ImgICO a) = ImgICO . histInject a


-- -----------------------------------------------------------------------------
-- * Utility routines

-- -------------------------------------
{-
...
-}
palInjector :: [RGBQuad]        -- ^ The original Palette
            -> [Int]            -- ^ Histogram of the image
            -> [B.ByteString]   -- ^ Data to be injected
            -> [RGBQuad]        -- ^ Returning a new Palette
palInjector (p:ps) (h:hs) (bs:bss) =
  if h /= 0
  then p          : palInjector ps hs (bs:bss)
  else bsToRGB' p bs : palInjector ps hs bss
  --else bsToRGB bs : palInjector ps hs bss
palInjector ps _ [] = ps
palInjector [] _ _  = []

null3 = B.take 3 $ B.repeat 0x00
--null3s = repeat null3

-- -------------------------------------
{- |
...
-}
palExtractor  :: [RGBQuad]      -- ^ Palette
              -> [Int]          -- ^ Histogram
              -> B.ByteString   -- ^ Extracted data
palExtractor ps hs = B.concat $ untilNull3 $ map (rgbToBS.fst) $ filter (\(p,h) -> h==0) $ zip ps hs
  where
    untilNull3 = takeWhile (/= null3)

-- -------------------------------------

bsToRGB :: B.ByteString -> RGBQuad
bsToRGB bs = RGBQuad r g b 0x00
  where (r:g:b:_) = B.unpack bs

bsToRGB' :: RGBQuad -> B.ByteString -> RGBQuad
bsToRGB' (RGBQuad _ _ _ a) bs = RGBQuad r g b a
  where (r:g:b:_) = B.unpack bs

-- -------------------------------------

rgbToBS :: RGBQuad -> B.ByteString
rgbToBS (RGBQuad r g b _) = B.pack [r,g,b]

-- -----------------------------------------------------------------------------
{-
Because icons can have different image types, even bmps with different
bit-counts, the palettes to be injected into maybe different sizes or
discontiguous.  So we need to prepare the injected data by breaking
into appropriate palette-sized chunks.
-}
partPack :: Icon -> B.ByteString -> [B.ByteString]
partPack ico = partitionByB (map histSpace $ iconImgs ico) . appendNulls
  where
    appendNulls = flip B.append (B.repeat 0x00)

-- -------------------------------------

partitionByB :: [Int] -> B.ByteString -> [B.ByteString]
partitionByB [] xs     = []
partitionByB (y:ys) xs = 
    if B.null b
    then [a]
    else a : partitionByB ys b
  where (a,b) = B.splitAt (fromIntegral y) xs

-- -------------------------------------
{-
partitionBy :: [Int] -> [a] -> [[a]]
partitionBy ys []     = []
partitionBy [] xs     = [xs]
partitionBy (y:ys) xs = a : partitionBy ys b
  where (a,b) = splitAt y xs
-}
-- -----------------------------------------------------------------------------
{-
Chunk the bytes up.  A bit overkeen in padding with nulls.
-}
chunkB :: Int64 -> B.ByteString -> [B.ByteString]
chunkB n bs = case (B.null bs) of
                True  -> []
                False -> (ls) : chunkB n rs
  where ls = pad0 $ B.take n bs
        rs = B.drop n bs
        pad0 :: B.ByteString -> B.ByteString
        pad0 = B.take n . flip B.append (B.repeat 0x00)


-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
