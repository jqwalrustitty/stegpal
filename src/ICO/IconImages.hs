{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : ICO.IconImages
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental
-}
-- -----------------------------------------------------------------------------

module ICO.IconImages
(
  IconImage(..)
)
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import BMP.DIBHeader
import BMP.Palette


-- -----------------------------------------------------------------------------
{- |
/For now, only interested in DIB (or BMP) type images./

Note that the 'icHeader' is imported from 'BMP.DIBHeader'.
The only real addition to the standard BMP image is the inclusion
of the 'icAND' mask which defines the /transparency/ of the icon.
-}
data IconImage = IconImage
  { icHeader  :: DIBHeader        -- ^ DIB header
  , icColors  :: [RGBQuad]        -- ^ Color table / Palette
  , icXOR     :: B.ByteString     -- ^ DIB bits for XOR mask
  , icAND     :: B.ByteString     -- ^ DIB bits for AND mask
  } deriving (Show, Read, Eq)

instance Binary IconImage where
  get = do
    dib <- get
    pal <- getPaletteDIB dib
    let imgSize = iconImgSize dib
    img1 <- isolate (imgSize) getRemainingLazyByteString
    img2 <- getRemainingLazyByteString
    return $! IconImage dib pal img1 img2
  put img = do
    put $ icHeader img
    put $ icColors img
    putLazyByteString $ icXOR img
    putLazyByteString $ icAND img
  putList = mapM_ put

-- -------------------------------------

iconImgSize :: DIBHeader -> Int
iconImgSize dib =
    if imgSize == 0
    then imgSize `div` 2
    else imgSize
  where imgSize = fromIntegral $ dibImgSize dib

-- -----------------------------------------------------------------------------
{- 
> typdef struct
> {
>   BITMAPINFOHEADER  icHeader;      // DIB header
>   RGBQUAD           icColors[1];   // Color table
>   BYTE              icXOR[1];      // DIB bits for XOR mask
>   BYTE              icAND[1];      // DIB bits for AND mask
> } ICONIMAGE, *LPICONIMAGE;
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
