{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : StegPal.Magic
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental
-}
-- -----------------------------------------------------------------------------

module StegPal.Magic
(
  Image(..)
--, magicPrefixOf
--, MagicType
)
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Binary.Utils
import BMP.Bitmap
import ICO.Icons


-- -----------------------------------------------------------------------------
{- |
Wrapper for images so that they work with 'StegPal.HistInjectable'.

Uses /magic/ bytes to determine image type.
-}
data Image = ImgBMP BMP | ImgICO Icon | ImgPNG B.ByteString | ImgUnknown B.ByteString
    deriving (Read, Eq)
--    deriving (Show, Read, Eq)

-- | Just shows a /shorthand/ of the file type, e.g., @BMP@.
instance Show Image where
  show (ImgBMP _) = "BMP"
  show (ImgICO _) = "Icon"
  show (ImgPNG _) = "PNG"
  show (ImgUnknown _) = "Unknown"

instance Binary Image where
    put (ImgBMP a) = put a
    put (ImgICO a) = put a
    put (ImgPNG a) = put a
    put (ImgUnknown a) = put a
    get = do  bs <- lookAhead (getLazyByteString 8)
              let itype = magicPrefixOf bs
              case itype of
                BMPfile       -> ImgBMP <$> get
                ICOfile       -> ImgICO <$> get
                PNGfile       -> ImgPNG <$> get
                UnknownImg i  -> ImgUnknown <$> get


-- -----------------------------------------------------------------------------

bmpMagic = 0x424d :: Word16
icoMagic = 0x00000100 :: Word32
pngMagic = 0x89504e470d0a1a0a :: Word64

bmpMagicB = B.pack [ 0x42, 0x4d ] :: B.ByteString
icoMagicB = B.pack [ 0x00, 0x00, 0x01, 0x00 ] :: B.ByteString
pngMagicB = B.pack [ 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a ] :: B.ByteString

-- -------------------------------------
{-
Image types 
-}
data MagicType = BMPfile | ICOfile | PNGfile | UnknownImg B.ByteString
    deriving (Show, Read, Eq)

-- -------------------------------------
{-
Determine the image type from the /magic/ bytes.
-}
magicPrefixOf :: B.ByteString -> MagicType
magicPrefixOf bs
    | B.isPrefixOf pngMagicB bs = PNGfile
    | B.isPrefixOf icoMagicB bs = ICOfile
    | B.isPrefixOf bmpMagicB bs = BMPfile
    | otherwise                 = UnknownImg bs

{-
getMagic2 :: Get MagicType
getMagic2 = do
    bs <- lookAhead (getLazyByteString 8)
    return $ magicPrefixOf bs
-}
-- -----------------------------------------------------------------------------
{- TODO:
* zlib compression starts with 2 "magic" bytes
  - magic bytes are:
    . 0x789c - default compression
    . 0x7801 - no compression
    . 0x78da - best compression
  - we are just using "default" so could strip those two bytes
    and add them when reconstructing
  - the colours are somewhere between green and blue 
    depending on the next byte
  - removing it, lowers the chance of detection
-}
-- -------------------------------------

zlibMagic = 0x789c :: Word16
zlibMagicB = B.pack $ [0x78, 0x9c] :: B.ByteString

addZlibMagic :: B.ByteString -> B.ByteString
addZlibMagic = B.append zlibMagicB

delZlibMagic :: B.ByteString -> B.ByteString
delZlibMagic bs = if B.isPrefixOf zlibMagicB bs
                  then B.drop 2 bs
                  else bs

-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
