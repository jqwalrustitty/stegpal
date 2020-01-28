{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : BMP.DIBHeader
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : experimental
-}
-- -----------------------------------------------------------------------------

module BMP.DIBHeader
{-
(
  DIBHeader(..)
, clrUsed
, dibImgSize
, bmpInfoHeaderSize
)
-}
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative 
import GHC.Int

import BMP.CIEXYZ
import BMP.Compression

-- -----------------------------------------------------------------------------
{- |
/Device Independant Bitmap/ definitions.

We get away with the field names being identical across the different
constructors, but need to be careful about 'dibClrUsed', which is not used by
'BMPCoreHeader', and 'dibBitCount', which must be limited to 1, 4, 8 or 24
in the 'BMPCoreHeader'.  These fields are used in determinig whether the image
uses a palette or not, as well as the size of the palette.
-}
data DIBHeader  =
  -- | Core Header (16 bytes)
    BMPCoreHeader { dibSize           :: Int32
                  , dibWidth          :: Int32
                  , dibHeight         :: Int32
                  , dibPlanes         :: Word16
                  , dibBitCount       :: Int16
                  }

  -- | Info Header (40 bytes)
  | BMPInfoHeader { dibSize           :: Int32  -- ^ The number of bytes required by this structure.
                  , dibWidth          :: Int32  -- ^ The width of the bitmap, in pixels.
                  , dibHeight         :: Int32  -- ^ The height of the bitmap, in pixels.
                  , dibPlanes         :: Word16 -- ^ The number of planes for the target device. Must be 1.
                  , dibBitCount       :: Int16  -- ^ The number of bits-per-pixel. Must be 1, 4, 8, 16, 24 or 32.
                  , dibCompression    :: Compression
                  , dibSizeImage      :: Word32 -- ^ The size, in bytes, of the image
                  , dibXPelsPerMeter  :: Int32
                  , dibYPelsPerMeter  :: Int32
                  , dibClrUsed        :: Word32 -- ^ The number of color indexes in the color table
                  , dibClrImportant   :: Word32
                  }

  -- | V4 Header (108 bytes)
  | BMPV4Header   { dibSize           :: Int32
                  , dibWidth          :: Int32
                  , dibHeight         :: Int32
                  , dibPlanes         :: Word16
                  , dibBitCount       :: Int16
                  , dibCompression    :: Compression
                  , dibSizeImage      :: Word32
                  , dibXPelsPerMeter  :: Int32
                  , dibYPelsPerMeter  :: Int32
                  , dibClrUsed        :: Word32
                  , dibClrImportant   :: Word32
                  , dibRedMask        :: Word32
                  , dibGreenMask      :: Word32
                  , dibBlueMask       :: Word32
                  , dibAlphaMask      :: Word32
                  , dibCSType         :: Word32
                  , dibEndpoints      :: CIEXYZTRIPLE
                  , dibGammaRed       :: Word32
                  , dibGammaGreen     :: Word32
                  , dibGammaBlue      :: Word32
                  }

  -- | V5 Header (124 bytes)
  | BMPV5Header   { dibSize           :: Int32
                  , dibWidth          :: Int32
                  , dibHeight         :: Int32
                  , dibPlanes         :: Word16
                  , dibBitCount       :: Int16
                  , dibCompression    :: Compression
                  , dibSizeImage      :: Word32
                  , dibXPelsPerMeter  :: Int32
                  , dibYPelsPerMeter  :: Int32
                  , dibClrUsed        :: Word32
                  , dibClrImportant   :: Word32
                  , dibRedMask        :: Word32
                  , dibGreenMask      :: Word32
                  , dibBlueMask       :: Word32
                  , dibAlphaMask      :: Word32
                  , dibCSType         :: Word32
                  , dibEndpoints      :: CIEXYZTRIPLE
                  , dibGammaRed       :: Word32
                  , dibGammaGreen     :: Word32
                  , dibGammaBlue      :: Word32
                  , dibIntent         :: Word32
                  , dibProfileData    :: Word32
                  , dibProfileSize    :: Word32
                  , dibReserved       :: Word32
                 }
  deriving (Show, Read, Eq)

-- -----------------------------------------------------------------------------

bmpCoreHeaderSize = 4 + 4 + 4 + 2 + 2 :: Int32
bmpInfoHeaderSize = bmpCoreHeaderSize + 4 + 4 + 4 + 4 + 4 + 4
bmpV4HeaderSize   = bmpInfoHeaderSize + 4 + 4 + 4 + 4 + 4 + (4+4+4)*3 + 4 + 4 + 4
bmpV5HeaderSize   = bmpV4HeaderSize   + 4 + 4 + 4 + 4

sizeOfDIBHeader dib = fromIntegral $ dibSize dib

-- -------------------------------------

--numColours dib =  2 ^ $ fromIntegral $ dibBitCount dib

clrUsed dib@(BMPCoreHeader {})  = 0
clrUsed dib = dibClrUsed dib

--bmpCompression dib@(BMPCoreHeader {})  = CompressionUnknown 0x00
--bmpCompression dib  = dibCompression dib

-- -------------------------------------

imgSizeFromDIB dib@(BMPCoreHeader {}) = fromIntegral $ dibWidth dib * dibHeight dib
imgSizeFromDIB dib                    = fromIntegral $ dibSizeImage dib

dibImgSize :: DIBHeader -> Integer
dibImgSize dib =
    if imgSize == 0
    then fromIntegral $ toInteger $ ceiling $ xySize / (8 / bitCount)
    else imgSize
  where imgSize   = fromIntegral $ imgSizeFromDIB dib
        xySize    = fromIntegral $ dibWidth dib * dibHeight dib
        bitCount  = fromIntegral $ dibBitCount dib

-- -----------------------------------------------------------------------------

instance Binary DIBHeader where
  get = 
    (do 16 <- lookAhead getWord8
        BMPCoreHeader <$> getInt32le  <*> getInt32le  <*> getInt32le  <*> getWord16le <*> getInt16le
    ) <|>
    (do 40 <- lookAhead getWord8
        BMPInfoHeader <$> getInt32le  <*> getInt32le  <*> getInt32le  <*> getWord16le <*> getInt16le
                      <*> get         <*> getWord32le <*> getInt32le  <*> getInt32le  <*> getWord32le <*> getWord32le
    ) <|>
    (do 108 <- lookAhead getWord8
        BMPV4Header <$> getInt32le  <*> getInt32le  <*> getInt32le  <*> getWord16le <*> getInt16le
                    <*> get         <*> getWord32le <*> getInt32le  <*> getInt32le  <*> getWord32le <*> getWord32le
                    <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le
                    <*> get         <*> getWord32le <*> getWord32le <*> getWord32le
    ) <|>
    (do 124 <- lookAhead getWord8
        BMPV5Header <$> getInt32le  <*> getInt32le  <*> getInt32le  <*> getWord16le <*> getInt16le
                    <*> get         <*> getWord32le <*> getInt32le  <*> getInt32le  <*> getWord32le <*> getWord32le
                    <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le
                    <*> get         <*> getWord32le <*> getWord32le <*> getWord32le
                    <*> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le
    ) <|> 
    (error "BMP.DIBHeader.get: unhandled header size")

  put dh =
    case (sizeOfDIBHeader dh) of
      16  ->   (putInt32le  $ dibSize dh)
            *> (putInt32le  $ dibWidth dh)
            *> (putInt32le  $ dibHeight dh)
            *> (putWord16le $ dibPlanes dh)
            *> (putInt16le  $ dibBitCount dh)

      40  ->   (putInt32le  $ dibSize dh)
            *> (putInt32le  $ dibWidth dh)
            *> (putInt32le  $ dibHeight dh)
            *> (putWord16le $ dibPlanes dh)
            *> (putInt16le  $ dibBitCount dh)
            *> (put         $ dibCompression dh)
            *> (putWord32le $ dibSizeImage dh)
            *> (putInt32le  $ dibXPelsPerMeter dh)
            *> (putInt32le  $ dibYPelsPerMeter dh)
            *> (putWord32le $ dibClrUsed dh)
            *> (putWord32le $ dibClrImportant dh)

      108 ->   (putInt32le  $ dibSize dh)
            *> (putInt32le  $ dibWidth dh)
            *> (putInt32le  $ dibHeight dh)
            *> (putWord16le $ dibPlanes dh)
            *> (putInt16le  $ dibBitCount dh)
            *> (put         $ dibCompression dh)
            *> (putWord32le $ dibSizeImage dh)
            *> (putInt32le  $ dibXPelsPerMeter dh)
            *> (putInt32le  $ dibYPelsPerMeter dh)
            *> (putWord32le $ dibClrUsed dh)
            *> (putWord32le $ dibClrImportant dh)
            *> (putWord32le $ dibRedMask dh)
            *> (putWord32le $ dibGreenMask dh)
            *> (putWord32le $ dibBlueMask dh)
            *> (putWord32le $ dibAlphaMask dh)
            *> (putWord32le $ dibCSType dh)
            *> (put         $ dibEndpoints dh)
            *> (putWord32le $ dibGammaRed dh)
            *> (putWord32le $ dibGammaGreen dh)
            *> (putWord32le $ dibGammaBlue dh)

      124 ->   (putInt32le  $ dibSize dh)
            *> (putInt32le  $ dibWidth dh)
            *> (putInt32le  $ dibHeight dh)
            *> (putWord16le $ dibPlanes dh)
            *> (putInt16le  $ dibBitCount dh)
            *> (put         $ dibCompression dh)
            *> (putWord32le $ dibSizeImage dh)
            *> (putInt32le  $ dibXPelsPerMeter dh)
            *> (putInt32le  $ dibYPelsPerMeter dh)
            *> (putWord32le $ dibClrUsed dh)
            *> (putWord32le $ dibClrImportant dh)
            *> (putWord32le $ dibRedMask dh)
            *> (putWord32le $ dibGreenMask dh)
            *> (putWord32le $ dibBlueMask dh)
            *> (putWord32le $ dibAlphaMask dh)
            *> (putWord32le $ dibCSType dh)
            *> (put         $ dibEndpoints dh)
            *> (putWord32le $ dibGammaRed dh)
            *> (putWord32le $ dibGammaGreen dh)
            *> (putWord32le $ dibGammaBlue dh)
            *> (putWord32le $ dibIntent dh)
            *> (putWord32le $ dibProfileData dh)
            *> (putWord32le $ dibProfileSize dh)
            *> (putWord32le $ dibReserved dh)

-- =============================================================================
{-

typedef struct tagBITMAPCOREHEADER {
  DWORD bcSize;
  WORD  bcWidth;
  WORD  bcHeight;
  WORD  bcPlanes;
  WORD  bcBitCount;
} BITMAPCOREHEADER, *PBITMAPCOREHEADER;

typedef struct tagBITMAPINFOHEADER {
  DWORD biSize;
  LONG  biWidth;
  LONG  biHeight;
  WORD  biPlanes;
  WORD  biBitCount;
  DWORD biCompression;
  DWORD biSizeImage;
  LONG  biXPelsPerMeter;
  LONG  biYPelsPerMeter;
  DWORD biClrUsed;
  DWORD biClrImportant;
} BITMAPINFOHEADER, *PBITMAPINFOHEADER;

typedef struct {
  DWORD        bV4Size;
  LONG         bV4Width;
  LONG         bV4Height;
  WORD         bV4Planes;
  WORD         bV4BitCount;
  DWORD        bV4V4Compression;
  DWORD        bV4SizeImage;
  LONG         bV4XPelsPerMeter;
  LONG         bV4YPelsPerMeter;
  DWORD        bV4ClrUsed;
  DWORD        bV4ClrImportant;
  DWORD        bV4RedMask;
  DWORD        bV4GreenMask;
  DWORD        bV4BlueMask;
  DWORD        bV4AlphaMask;
  DWORD        bV4CSType;
  CIEXYZTRIPLE bV4Endpoints;
  DWORD        bV4GammaRed;
  DWORD        bV4GammaGreen;
  DWORD        bV4GammaBlue;
} BITMAPV4HEADER, *PBITMAPV4HEADER;

typedef struct {
  DWORD        bV5Size;
  LONG         bV5Width;
  LONG         bV5Height;
  WORD         bV5Planes;
  WORD         bV5BitCount;
  DWORD        bV5Compression;
  DWORD        bV5SizeImage;
  LONG         bV5XPelsPerMeter;
  LONG         bV5YPelsPerMeter;
  DWORD        bV5ClrUsed;
  DWORD        bV5ClrImportant;
  DWORD        bV5RedMask;
  DWORD        bV5GreenMask;
  DWORD        bV5BlueMask;
  DWORD        bV5AlphaMask;
  DWORD        bV5CSType;
  CIEXYZTRIPLE bV5Endpoints;
  DWORD        bV5GammaRed;
  DWORD        bV5GammaGreen;
  DWORD        bV5GammaBlue;
  DWORD        bV5Intent;
  DWORD        bV5ProfileData;
  DWORD        bV5ProfileSize;
  DWORD        bV5Reserved;
} BITMAPV5HEADER, *PBITMAPV5HEADER;
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
