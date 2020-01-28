{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : StegPal.RGBAlpha
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental

Currently unused module that will use the 'rgbReserved' field of 'RGBQuad' in
the palette to stuff data.

Assumes that the 'rgbReserved' value is @0x00@.
-}
-- -----------------------------------------------------------------------------

module StegPal.RGBAlpha
(
  AlphaInjectable(..)
)
where

import qualified Data.ByteString.Lazy as B

import BMP.Bitmap
import ICO.Icons

--import StegPal.Encoder
import StegPal.Magic

import Binary.Utils
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word


-- -----------------------------------------------------------------------------
{- |
Class for injecting into the 'rgbReserved' (or /Alpha/) channel of palette
colours.
-}
class AlphaInjectable image where
  -- | Image palette accessor
  alphaPalette :: image -> [RGBQuad]

  -- | Bit Count for image.  Essentially log_2 of the number of colours.
  alphaBits    :: Integral n => image -> n

  -- | How many bytes can we inject into the palette.
  alphaSpace   :: image -> Int
  alphaSpace   = (^) 2 . alphaBits

  -- | Injects 'ByteString' into the image, and returns the new image.
  alphaInject  :: image -> B.ByteString -> image

  -- | Extracts the data from the palette of the image.
  alphaExtract :: image -> B.ByteString
  alphaExtract = B.pack . bytesFromPalette . alphaPalette

-- -------------------------------------

inject :: [RGBQuad] -> B.ByteString -> [RGBQuad]
inject pal bs = zipWith updateQuadAlpha pal ws
  where 
    ws = (B.unpack bs) ++ repeat 0x00

-- -------------------------------------

extract :: [RGBQuad] -> B.ByteString
extract = B.pack . bytesFromPalette

-- -------------------------------------

bytesFromPalette :: [RGBQuad] -> [Word8]
bytesFromPalette = map rgbReserved

-- -------------------------------------

updateQuadAlpha :: RGBQuad -> Word8 -> RGBQuad
updateQuadAlpha rgb q = rgb { rgbReserved = q }
--updateQuadAlpha (RGBQuad r g b a) q = RGBQuad r g b q


-- -----------------------------------------------------------------------------

instance AlphaInjectable BMP where
  alphaPalette  = bmpPalette
  alphaBits     = fromIntegral . dibBitCount . bmpDIBHeader
  --alphaSpace    = fromIntegral . length . filter (\(RGBQuad _ _ _ a) -> a==0x00) . alphaPalette
  alphaInject img bs  = img {bmpPalette = (inject (alphaPalette img) bs) }

-- -------------------------------------

instance AlphaInjectable IconImage where
  alphaPalette  = icColors
  alphaBits     = fromIntegral . dibBitCount . icHeader
  alphaInject img bs  = img { icColors = (inject (alphaPalette img) bs) }

-- -------------------------------------

instance AlphaInjectable Icon where
  alphaPalette  = undefined
  alphaBits     = undefined
  alphaSpace    = sum . map alphaSpace . iconImgs
  alphaExtract  = B.concat . map alphaExtract . iconImgs
  alphaInject img bs = img { iconImgs = newImgs }
    where
      newImgs = zipWith alphaInject (iconImgs img) (partPack img bs)

-- -------------------------------------

instance AlphaInjectable Image where
  alphaPalette (ImgBMP a) = alphaPalette a
  alphaPalette (ImgICO a) = alphaPalette a

  alphaBits (ImgBMP a)    = alphaBits a
  alphaBits (ImgICO a)    = alphaBits a

  alphaSpace (ImgBMP a)    = alphaSpace a
  alphaSpace (ImgICO a)    = alphaSpace a

  alphaExtract (ImgBMP a) = alphaExtract a
  alphaExtract (ImgICO a) = alphaExtract a

  alphaInject (ImgBMP a) = ImgBMP . alphaInject a
  alphaInject (ImgICO a) = ImgICO . alphaInject a

-- -------------------------------------
{-
Because icons can have different image types, even bmps with different
bit-counts, the palettes to be injected into maybe different sizes or
discontiguous.  So we need to prepare the injected data by breaking
into appropriate palette-sized chunks.
-}
partPack :: Icon -> B.ByteString -> [B.ByteString]
partPack ico = partitionByB (map alphaSpace $ iconImgs ico) . appendNulls
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

-- =============================================================================
{-
alphaInjectB :: [RGBQuad] -> B.ByteString -> [RGBQuad]
alphaInjectB pal bs = zipWith (\q u -> updateQuadAlpha q u) pal (ws ++ repeat 0x00)
  where
    ws = getGenericList getWord8 $ packer bs

alphaExtractB :: [RGBQuad] -> B.ByteString
alphaExtractB = unpacker . runPut . putListSimple . bytesFromPalette

-- -------------------------------------

alphaInject_BMP :: BMP -> B.ByteString -> BMP
alphaInject_BMP bmp ccs = bmp {bmpPalette = newPalette}
  where
    newPalette = alphaInjectB (bmpPalette bmp) ccs

alphaExtract_BMP :: BMP -> B.ByteString
alphaExtract_BMP = alphaExtractB . bmpPalette

-- -----------------------------------------------------------------------------
-- -------------------------------------

alphaInject :: [RGBQuad] -> [Word8] -> [RGBQuad]
alphaInject pal ws = zipWith (\q u -> updateQuadAlpha q u) pal (ws ++ repeat 0x00)

alphaExtract :: [RGBQuad] -> [Word8]
alphaExtract = bytesFromPalette
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
