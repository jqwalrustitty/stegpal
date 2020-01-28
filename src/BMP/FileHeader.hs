{-X LANGUAGE DeriveGeneric #-}
{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : BMP.FileHeader
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : experimental

Extracts File Header information from Bitmaps.
-}
-- -----------------------------------------------------------------------------

module BMP.FileHeader
(
  BMPFileHeader(..)
, bmpHeaderSize
)
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

-- -----------------------------------------------------------------------------

bmpHeaderSize :: Integral a => a
bmpHeaderSize = 14
bmpMagic = 0x4d42 :: Word16

-- -------------------------------------
{- |
Magic bytes, size and offset to the pixel data.
-}
data BMPFileHeader = BMPFileHeader
  { bfType      :: !Word16    -- ^ The file type; must be BM.
  , bfSize      :: !Word32    -- ^ The size, in bytes, of the bitmap file.
  , bfReserved1 :: !Word16    -- ^ Reserved; must be zero.
  , bfReserved2 :: !Word16    -- ^ Reserved; must be zero.
  , bfOffBits   :: !Word32    -- ^ The offset, in bytes to the bitmap bits.
  } deriving (Show, Read, Eq)

-- -------------------------------------

instance Binary BMPFileHeader where
  get     = BMPFileHeader <$> getWord16le <*> getWord32le <*> getWord16le <*> getWord16le <*> getWord32le
  put bf  = do  putWord16le $ bfType bf
                putWord32le $ bfSize bf
                putWord16le $ bfReserved1 bf
                putWord16le $ bfReserved2 bf
                putWord32le $ bfOffBits bf


-- -------------------------------------
{-
 - https://msdn.microsoft.com/en-us/library/windows/desktop/dd183374(v=vs.85).aspx

typedef struct tagBITMAPFILEHEADER {
  WORD  bfType;
  DWORD bfSize;
  WORD  bfReserved1;
  WORD  bfReserved2;
  DWORD bfOffBits;
} BITMAPFILEHEADER, *PBITMAPFILEHEADER;
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
