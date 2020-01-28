{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : BMP.Bitmap
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : experimental

Structures for reading and writing bitmap files.
-}
-- -----------------------------------------------------------------------------

module BMP.Bitmap
  (
  -- * Bitmaps
    module BMP.Bitmap
  -- ** File Header
  , module BMP.FileHeader
  -- ** Device Independant Bitmap Headers
  , module BMP.DIBHeader
  -- ** Compression
  , module BMP.Compression
  -- ** CIEXYZ
  , module BMP.CIEXYZ
  -- ** Palette
  , module BMP.Palette
  -- ** Pixels
  --
  -- Pixels themselves are only stored as 'B.ByteString'.
  , module BMP.Pixels
  )
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import BMP.FileHeader
import BMP.DIBHeader
import BMP.CIEXYZ
import BMP.Compression
import BMP.Palette
import BMP.Pixels

-- -----------------------------------------------------------------------------
{-
  https://www-user.tu-chemnitz.de/~heha/petzold/ch15b.htm
-}
-- -----------------------------------------------------------------------------
{- |
A bitmap consists of the file header, a DIB header, an optional palette
(which could have been Maybe, but is @[]@ if non existent)
and then the pixels.

Pixels are just stored as a raw 'B.ByteString' but are processed as 'Pix'
where necessary.
-}
data BMP = BMP  { bmpFileHeader :: BMPFileHeader
                , bmpDIBHeader  :: DIBHeader
                , bmpPalette    :: [RGBQuad]
                , bmpPixels     :: B.ByteString
                }
  deriving (Show,Read,Eq)

-- -------------------------------------

instance Binary BMP where
  get = do
    bf  <- get
    dib <- get
    pal <- getPaletteDIB dib

    pxs <- getLazyByteString (fromIntegral $ bfSize bf - bfOffBits bf)

    return $! BMP bf dib pal pxs

  put (BMP bf dib pal pxs) = 
       put bf
    *> put dib
    *> put pal
    *> putLazyByteString pxs

-- -------------------------------------

readBMP :: FilePath -> IO BMP
readBMP file = decodeFile file

writeBMP :: FilePath -> BMP -> IO ()
writeBMP file = encodeFile file

-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
