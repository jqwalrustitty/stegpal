{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : ICO.DirEntries
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental
-}
-- -----------------------------------------------------------------------------

module ICO.DirEntries
(
  IconDirEntry(..)
)
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Binary.Utils


-- -----------------------------------------------------------------------------
{- |
Description of the icon image in the icon file.

Note the fields 'dwImageOffset' and 'dwBytesInRes' which define
the offset and size of the image in the overall icon file.
-}
data IconDirEntry = IconDirEntry
  { bWidth        :: !Word8         -- ^ Width, in pixels, of the image
  , bHeight       :: !Word8         -- ^ Height, in pixels, of the image
  , bColorCount   :: !Word8         -- ^ Number of colors in image (0 if >=8bpp)
  , bReserved     :: !Word8         -- ^ Reserved ( must be 0)
  , wPlanes       :: !Word16        -- ^ Color Planes
  , wBitCount     :: !Word16        -- ^ Bits per pixel
  , dwBytesInRes  :: !Word32        -- ^ How many bytes in this resource?
  , dwImageOffset :: !Word32        -- ^ Where in the file is this image?
  } deriving (Show, Read, Eq)

instance Binary IconDirEntry where
  get         = IconDirEntry  <$> getWord8 <*> getWord8
                              <*> getWord8 <*> getWord8
                              <*> getWord16le <*> getWord16le
                              <*> getWord32le <*> getWord32le
  put dirent  = do  putWord8    $ bWidth dirent
                    putWord8    $ bHeight dirent
                    putWord8    $ bColorCount dirent
                    putWord8    $ bReserved dirent
                    putWord16le $ wPlanes dirent
                    putWord16le $ wBitCount dirent
                    putWord32le $ dwBytesInRes dirent
                    putWord32le $ dwImageOffset dirent
  putList xs  = mapM_ put xs

sizeOfDirEntry :: Integral a => a
sizeOfDirEntry = 16
--sizeOfDirEntry = 1 + 1 + 1 + 1 + 2 + 2 + 4 + 4

-- -----------------------------------------------------------------------------
{-
An Icon Header basically has a couple of /magic bytes/ followed by a
'icoCount' of the number of entries.  Then a list of the 'IconDir' entries
-}
{-
data IconDir = IconDir
  { icoReserved :: !Word16          -- ^ Reserved (must be 0)
  , icoType     :: !Word16          -- ^ Resource Type (1 for icons)
  , icoCount    :: !Word16          -- ^ How many images?
  , icoEntries  :: [IconDirEntry]   -- ^ An entry for each image (icoCount of 'em)
  } deriving (Show, Read, Eq)

instance Binary IconDir where
  get = do
    rsv <- getWord16le
    typ <- getWord16le
    cnt <- getWord16le
    ents <- isolate (fromIntegral cnt * sizeOfDirEntry) getListSimple
    return $! IconDir rsv typ cnt ents

  put icod  = do  putWord16le $ icoReserved icod
                  putWord16le $ icoType icod
                  putWord16le $ icoCount icod
                  put         $ icoEntries icod
-}
-- -----------------------------------------------------------------------------
{-
typedef struct
{
    WORD           idReserved;   // Reserved (must be 0)
    WORD           idType;       // Resource Type (1 for icons)
    WORD           idCount;      // How many images?
    ICONDIRENTRY   idEntries[1]; // An entry for each image (idCount of 'em)
} ICONDIR, *LPICONDIR;

typedef struct
{
    BYTE        bWidth;          // Width, in pixels, of the image
    BYTE        bHeight;         // Height, in pixels, of the image
    BYTE        bColorCount;     // Number of colors in image (0 if >=8bpp)
    BYTE        bReserved;       // Reserved ( must be 0)
    WORD        wPlanes;         // Color Planes
    WORD        wBitCount;       // Bits per pixel
    DWORD       dwBytesInRes;    // How many bytes in this resource?
    DWORD       dwImageOffset;   // Where in the file is this image?
} ICONDIRENTRY, *LPICONDIRENTRY;
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
