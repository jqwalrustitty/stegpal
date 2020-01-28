{-# OPTIONS_HADDOCK prune, hide #-}
-- -----------------------------------------------------------------------------
{- |
Module        : StegPal.Histogram
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental
-}
-- -----------------------------------------------------------------------------

module StegPal.Histogram where

import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V

import qualified Data.ByteString.Lazy as B
import Data.Word

-- -----------------------------------------------------------------------------

vectorCountN :: (Integral a) => Int -> [a] -> [Int]
vectorCountN n bs = V.toList $ V.create $ do
  v <- M.new n
  mapM_ (\i -> M.unsafeWrite v i 0) [0..(n-1)]
  mapM_ (\i -> M.unsafeRead v (fromIntegral i) >>= M.unsafeWrite v (fromIntegral i) . (+1) ) bs
  return v

vectorCountB :: B.ByteString -> [Int]
vectorCountB = vectorCountN 256 . B.unpack

vectorCount :: B.ByteString -> [Int]
vectorCount = vectorCountB

-- -------------------------------------

vectorCountSafe :: Integral a => Int -> [a] -> [Int]
vectorCountSafe bitCount bs = 
  if (bitCount `elem` [1, 4, 8])
  then vectorCountN (2^bitCount) bs
  else []


-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
