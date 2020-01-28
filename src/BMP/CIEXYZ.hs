{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : BMP.CIEXYZ
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : experimental
-}
-- -----------------------------------------------------------------------------

module BMP.CIEXYZ
(
  CIEXYZ(..)
, CIEXYZTRIPLE(..)
)
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

-- -----------------------------------------------------------------------------
{- |
Nobody has any idea what these do.
-}
data CIEXYZ = CIEXYZ Word32 Word32 Word32
  deriving (Show,Read,Eq)

{- |
Nobody has any idea what these do.
-}
data CIEXYZTRIPLE = CIEXYZTRIPLE CIEXYZ CIEXYZ CIEXYZ
  deriving (Show,Read,Eq)

-- -------------------------------------

instance Binary CIEXYZ where
  get = CIEXYZ <$> getWord32le <*> getWord32le <*> getWord32le
  put (CIEXYZ a b c)
      = do  putWord32le a
            putWord32le b
            putWord32le c

instance Binary CIEXYZTRIPLE where
  get = CIEXYZTRIPLE <$> get <*> get <*> get
  put (CIEXYZTRIPLE a b c) 
      = do  put a
            put b
            put c

-- -----------------------------------------------------------------------------
{-
 - https://msdn.microsoft.com/en-us/library/windows/desktop/dd371833(v=vs.85).aspx
typedef struct tagCIEXYZTRIPLE {
  CIEXYZ ciexyzRed;
  CIEXYZ ciexyzGreen;
  CIEXYZ ciexyzBlue;
} CIEXYZTRIPLE typedef CIEXYZTRIPLE FAR* LPCIEXYZTRIPLE;

 - https://msdn.microsoft.com/en-us/library/windows/desktop/dd371828(v=vs.85).aspx
typedef struct tagCIEXYZ {
  FXPT2DOT30 ciexyzX;
  FXPT2DOT30 ciexyzY;
  FXPT2DOT30 ciexyzZ;
} CIEXYZ, CIEXYZ FAR* LPCIEXYZ;
-}
-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
