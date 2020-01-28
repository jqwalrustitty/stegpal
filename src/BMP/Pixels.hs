{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : BMP.Pixels
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : experimental
-}
-- -----------------------------------------------------------------------------

module BMP.Pixels
(
  Pix
, toPixels
, toPixelsRaw
, fromPixels
, mapToPix
)
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import Data.Bits
import Data.BitVector

import Binary.Utils
import BMP.DIBHeader

-- -----------------------------------------------------------------------------
{-
RowLength = 4 * ((bmch.bcWidth * bmch.bcBitCount + 31) / 32) ;
 or
RowLength = ((bmch.bcWidth * bmch.bcBitCount + 31) & ~31) >> 3 ;

For DIBs with 1 bit per pixel, each byte corresponds to 8 pixels.
The leftmost pixel is the most-significant bit of the first byte:

For DIBs with 4 bits per pixel, each byte corresponds to 2 pixels.
The leftmost pixel is the high 4 bits of the first byte

For a DIB with 8 bits per pixel, each byte is 1 pixel:

For DIBs with 24 bits-per-pixel, each pixel requires 3 bytes for
the red, green, and blue color values. Each row of pixel bits is
basically an array of RGBTRIPLE structures, possibly padded with
0 bytes at the end of each row so that the row has a multiple of 4 bytes
-}
-- -----------------------------------------------------------------------------

data RGBTriple = RGBTriple  { rgbtBlue  :: !Word8
                            , rgbtGreen :: !Word8
                            , rgbtRed   :: !Word8
                            }
  deriving (Show, Read, Eq)

instance Binary RGBTriple where
  get = RGBTriple <$> getWord8 <*> getWord8 <*> getWord8
  put (RGBTriple r g b)
      = do  putWord8 r
            putWord8 g
            putWord8 b

-- -----------------------------------------------------------------------------
{- |
Oversimplified representation of pixel arrays.
-}
data Pix = Pix8 !Word8
         | Pix4 !Word8
         | Pix2 !Word8
         | Pix1 !Bool
  deriving (Read, Eq)

instance Show Pix where
  show (Pix8 a) = "Pix8 " ++ asHex a
  show (Pix4 a) = "Pix4 " ++ asHex a
  show (Pix2 a) = "Pix2 " ++ asHex a

  show (Pix1 a) = "Pix1 " ++ show a
  --show (Pix1 a) = "Pix1 " ++ (asHex $ fromEnum a)
  --show (Pix1 a) = show $ fromEnum a

-- -----------------------------------------------------------------------------

toPix8 byte = Pix8 <$> toPix8' byte
toPix4 byte = Pix4 <$> toPix4' byte
toPix2 byte = Pix2 <$> toPix2' byte
toPix1 byte = Pix1 <$> toPix1' byte

toPix8' :: Word8 -> [Word8]
toPix8' byte = [byte]

toPix4' :: Word8 -> [Word8]
toPix4' byte = [a,b]
  where
    a = byte `shiftR` 4 .&. 0x0f
    b = byte `shiftR` 0 .&. 0x0f

toPix2' :: Word8 -> [Word8]
toPix2' byte = [a,b,c,d]
  where
    a = (byte `shiftR` 6) .&. 0x03
    b = (byte `shiftR` 4) .&. 0x03
    c = (byte `shiftR` 2) .&. 0x03
    d = (byte `shiftR` 0) .&. 0x03

toPix1' :: Bits a => a -> [Bool]
toPix1' byte = map (testBit byte) [7,6..0]

-- -------------------------------------

{- |
...
-}
toPixels :: (Integral a) => a -> [Word8] -> [Pix]
toPixels n ws
  | n==1  = Pix1 <$> concatMap toPix1' ws
  | n==2  = Pix2 <$> concatMap toPix2' ws
  | n==4  = Pix4 <$> concatMap toPix4' ws
  | n==8  = Pix8 <$> ws

{- |
Used by the 'HistInjectable' class to create the histogram.
-}
toPixelsRaw :: (Integral a) => a -> [Word8] -> [Word8]
toPixelsRaw n ws
  | n==1  = map (fromIntegral . fromEnum) $ concatMap toPix1' ws
  | n==2  = concatMap toPix2' ws
  | n==4  = concatMap toPix4' ws
  | n==8  = ws

{- |
Wrapper, cannot remember why.
-}
mapToPix :: (Integral a) => a -> [Word8] -> [Pix]
mapToPix n ws
  | n==1 = undefined -- Pix1 <$> (map (fromIntegral . fromEnum) ws)
  | n==2 = Pix2 <$> ws
  | n==4 = Pix4 <$> ws
  | n==8 = Pix8 <$> ws
  | otherwise = []


-- -----------------------------------------------------------------------------
{- |
Mostly only used by the Analysis module.
-}
fromPixels :: [Pix] -> [Word8]
fromPixels pxls@(Pix8 _:_) = map (\(Pix8 a) -> a) pxls
fromPixels pxls@(Pix4 _:_) = map (pixList fromPix4) $ chunkN 2 pxls
  where pixList f (a:b:_) = f a b
        fromPix4 (Pix4 a) (Pix4 b) = a*16 + b
fromPixels pxls@(Pix2 _:_) = map (pixList fromPix2) $ chunkN 4 pxls
  where pixList f (a:b:c:d:_) = f a b c d
        fromPix2 (Pix2 a) (Pix2 b) (Pix2 c) (Pix2 d) = a*4*4*4 + b*4*4 + c*4 + d
fromPixels pxls@(Pix1 _:_) = map (fromIntegral.nat.fromBits.map pixToBool) $ chunkN 8 pxls
  where pixToBool (Pix1 a) = a
fromPixels _ = []


-- -------------------------------------

chunkN :: Int -> [a] -> [[a]]
chunkN n xs = case null xs of
                True  -> []
                False -> (ls) : chunkN n rs
  where (ls,rs) = splitAt n xs

-- -------------------------------------




-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
