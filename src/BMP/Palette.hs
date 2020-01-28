{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : BMP.Palette
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : experimental
-}
-- -----------------------------------------------------------------------------

module BMP.Palette
(
  RGBQuad(..)
, getPaletteDIB
)
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Binary.Utils
import BMP.DIBHeader

-- -----------------------------------------------------------------------------
{-
typedef struct tagRGBQUAD {
  BYTE rgbBlue;
  BYTE rgbGreen;
  BYTE rgbRed;
  BYTE rgbReserved;
} RGBQUAD;

typedef struct tagRGBTRIPLE {
  BYTE rgbtBlue;
  BYTE rgbtGreen;
  BYTE rgbtRed;
} RGBTRIPLE;
-}
-- -----------------------------------------------------------------------------
{- |
The Palette is a list of 'RGBQuad', but it really only defines the RGB component.
The reserved field is unused and must be zero (possibly the /Alpha/ channel).
-}
data RGBQuad = RGBQuad  { rgbBlue     :: !Word8   -- ^ Blue
                        , rgbGreen    :: !Word8   -- ^ Green
                        , rgbRed      :: !Word8   -- ^ Red
                        , rgbReserved :: !Word8   -- ^ Must be zero
                        }
  deriving (Read, Eq)

instance Show RGBQuad where
  show (RGBQuad r g b a) = "RGBQuad " ++  asHex r ++ " " ++
                                          asHex g ++ " " ++
                                          asHex b ++ " " ++
                                          asHex a

instance Binary RGBQuad where
  get = RGBQuad <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
  put (RGBQuad r g b a)
      = do  putWord8 r
            putWord8 g
            putWord8 b
            putWord8 a
  putList xs = mapM_ put xs

-- -------------------------------------

getPaletteDIB :: DIBHeader -> Get [RGBQuad]
getPaletteDIB dib = isolate (paletteBytes dib) getListSimple

{-
getPaletteDIB :: DIBHeader -> Get [RGBQuad]
--getPaletteDIB dib = isolate (paletteBytes dib) getPalette

getPalette :: Get [RGBQuad]
getPalette = do
  empty <- isEmpty
  if empty
  then return []
  else do p   <- get
          ps  <- getPalette
          return (p:ps)
-}

--parsePalette :: DIBHeader -> B.ByteString -> [RGBQuad]
--parsePalette dib = runGet (isolate (paletteBytes dib) getPalette)

-- -------------------------------------

paletteBytes :: (Integral a) => DIBHeader -> a
paletteBytes = (*) 4 . paletteSize

paletteSize :: (Integral a) => DIBHeader -> a
paletteSize dib = fromIntegral $ 
  case dibBitCount dib of
    24  -> 0
    _   -> case clrUsed dib of
            0 -> 2^(dibBitCount dib)
            _ -> (clrUsed dib)

-- -----------------------------------------------------------------------------
{--}
newtype Palette = Palette [RGBQuad]

instance Binary Palette where
  get = undefined
  put = undefined
  putList = undefined
{--}
-- =============================================================================
{-
typedef struct tagRGBQUAD {
  BYTE rgbBlue;
  BYTE rgbGreen;
  BYTE rgbRed;
  BYTE rgbReserved;
} RGBQUAD;

typedef struct tagRGBTRIPLE {
  BYTE rgbtBlue;
  BYTE rgbtGreen;
  BYTE rgbtRed;
} RGBTRIPLE;
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
