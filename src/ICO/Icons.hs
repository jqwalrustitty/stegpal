{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : ICO.Icons
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental

Structures for reading and writing icon files.

Only deals with DIB\/BMP icon types.
-}
-- -----------------------------------------------------------------------------

module ICO.Icons
(
-- * Icons
  Icon(..)
-- * Icon Directory Entries
, module ICO.DirEntries
-- * Icon Images
, module ICO.IconImages
)
where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import ICO.DirEntries
import ICO.IconImages

import Binary.Utils

-- -----------------------------------------------------------------------------
{- | 
An Icon Header basically has a couple of /magic bytes/ followed by a
'icoCount' of the number of 'IconDirEntry', then a list of the
icon images.
-}
data Icon = Icon
  { icoReserved :: !Word16          -- ^ Reserved (must be 0)
  , icoType     :: !Word16          -- ^ Resource Type (1 for icons)
  , icoCount    :: !Word16          -- ^ How many images?
  , icoEntries  :: [IconDirEntry]   -- ^ An entry for each image (icoCount of 'em)
  , iconImgs    :: [IconImage]      -- ^ List of icon images (icoCount of 'em)
  } deriving (Show, Read, Eq)

instance Binary Icon where
  get = do
    rsv <- getWord16le
    typ <- getWord16le
    cnt <- getWord16le
    --ents <- isolate (fromIntegral cnt * sizeOfDirEntry) getListSimple
    ents <- isolate (fromIntegral cnt * 16) getListSimple

    let dirEnts = map (\x -> (fromIntegral $ dwImageOffset x
                             ,fromIntegral $ dwBytesInRes x)) $ ents
    imgs <- mapM (\(x,y) -> isolate (fromIntegral y) get :: Get IconImage ) dirEnts
    return $! Icon rsv typ cnt ents imgs

  put ico = do  putWord16le $ icoReserved ico
                putWord16le $ icoType ico
                putWord16le $ icoCount ico
                put         $ icoEntries ico
                put         $ iconImgs ico

-- -----------------------------------------------------------------------------
{-
data Icon = Icon
  { iconDir     :: IconDir        -- ^ Icon header
  , iconImgs    :: [IconImage]    -- ^ List of icon images
  } deriving (Show, Read, Eq)

instance Binary Icon where
  get = do
    dirHdr <- get
    let dirEnts = map (\x -> (fromIntegral $ dwImageOffset x
                             ,fromIntegral $ dwBytesInRes x)) $ icoEntries dirHdr
    imgs <- mapM (\(x,y) -> isolate (fromIntegral y) get :: Get IconImage ) dirEnts
    return $! Icon dirHdr imgs
  put icon = do put $ iconDir icon
                put $ iconImgs icon
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
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
