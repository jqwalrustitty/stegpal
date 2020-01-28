{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module       : Binary.Utils
Copyright    : (c) 2018
License      : BSD3
-}
-- -----------------------------------------------------------------------------

module Binary.Utils where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as BI
import qualified Data.ByteString as BS
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Numeric

-- -------------------------------------
clp=mapM_(\x->putStrLn[])[1..25]::IO()
-- -----------------------------------------------------------------------------

getGenericList :: Get a -> B.ByteString -> [a]
getGenericList getter bs = go decoder bs
  where
    --decoder :: Decoder a
    decoder = runGetIncremental getter

    --go :: Decoder a -> B.ByteString -> [a]
    go (Done leftover _consumed got) input    = got : go decoder (BI.chunk leftover input)
    go (Partial k) input                      = go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (Fail _leftover _consumed msg) _input  = []


takeHeadChunk :: B.ByteString -> Maybe BS.ByteString
takeHeadChunk lbs = case lbs of
    (BI.Chunk bs _) -> Just bs
    _               -> Nothing

dropHeadChunk :: B.ByteString -> B.ByteString
dropHeadChunk lbs = case lbs of
    (BI.Chunk _ lbs') -> lbs'
    _                 -> BI.Empty

-- -------------------------------------

getListSimple :: (Binary a) => Get [a]
getListSimple = do 
    empty <- isEmpty
    if empty
    then return []
    else do p   <- get
            ps  <- getListSimple
            return (p:ps)

putListSimple :: Binary a => [a] -> Put
putListSimple = mapM_ put


-- =============================================================================

type Hex    = String

-- -------------------------------------
-- | back and forth from Integers to Hex
toHex   :: Integer -> Hex
fromHex :: Hex -> Integer

toHex   = prepad . flip showHex ""
fromHex = fst . head . readHex

--asHex :: Integer -> Hex
asHex :: (Integral a) => a -> Hex
asHex = (++) "0x" . toHex . fromIntegral

-- -------------------------------------
-- | pads out hex strings with 0 at start
prepad :: Hex -> Hex
prepad = prepadTo 2

prepadTo :: Int -> Hex -> Hex
prepadTo q xs = (replicate n '0') ++ xs
  where n = (q - (length xs) `mod` q) `mod` q


-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
