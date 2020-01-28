{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- -----------------------------------------------------------------------------
{- |
Module        : StegPal.Encoder
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental
-}
-- -----------------------------------------------------------------------------

module StegPal.Encoder
(
  Packable(..)
)
where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Word              (Word8)
import GHC.Int                (Int64)
import Crypto.Cipher
import Codec.Compression.Zlib (compress, decompress)
import qualified Data.Text as T
import Data.Text.Encoding     (encodeUtf8, decodeUtf8)
import Data.Char              (chr, ord)


-- -----------------------------------------------------------------------------
{- |
The assumption is that the default 'Packable' type is going to be a ByteString,

By default, the packer will compress the data with 'Codec.Compression.Zlib'.
-}
class Packable a where
  -- | Convert to 'ByteString'
  packer    :: a -> B.ByteString
  -- | Convert from 'ByteString'
  unpacker  :: B.ByteString -> a

  -- | AES128 encryption where first parameter is the passphrase
  packerWithKey   :: String         -- ^ passphrase \/ key
                  -> a              -- ^ data to be packed
                  -> B.ByteString
  packerWithKey keyStr    = encipher keyStr . packer
  -- | AES128 decryption where first parameter is the passphrase
  unpackerWithKey :: String         -- ^ passphrase \/ key
                  -> B.ByteString   -- ^ data to be unpacked
                  -> a
  unpackerWithKey keyStr  = unpacker . decipher keyStr

  -- | With 'Nothing' just uses 'packer', otherwise uses 'packerWithKey'
  packerM   :: Maybe String         -- ^ Just passphrase \/ key
            -> a                    -- ^ data to be packed
            -> B.ByteString
  packerM = maybe packer packerWithKey

  -- | With 'Nothing' just uses 'unpacker', otherwise uses 'unpackerWithKey'
  unpackerM   :: Maybe String       -- ^ Just passphrase \/ key
              -> B.ByteString       -- ^ data to be unpacked
              -> a
  unpackerM = maybe unpacker unpackerWithKey

{-
packerM :: Packable a => Maybe String -> a -> B.ByteString
packerM Nothing     = packer
packerM (Just k)    = packerWithKey k

unpackerM :: Packable a => Maybe String -> B.ByteString -> a
unpackerM Nothing   = unpacker
unpackerM (Just k)  = unpackerWithKey k
-}

-- -------------------------------------

instance Packable B.ByteString where
  packer    = compress
  unpacker  = decompress

-- -------------------------------------
{- |
Basically, 'String'.
-}
instance a ~ Char => Packable [a] where
  packer    = packer . packStr
  unpacker  = unpackStr . unpacker

-- -------------------------------------

instance Packable T.Text where
  packer = packer . B.fromStrict . encodeUtf8
  unpacker = decodeUtf8 . B.toStrict . unpacker

-- -----------------------------------------------------------------------------

encipher :: String -> B.ByteString -> B.ByteString
encipher keyStr = withStrict (cbcEncrypt ctx nullIV) . pad
  where
    ctx = cipherInit key :: AES128
    Right key = makeKey (B.toStrict $ pad $ packStr keyStr)
    pad = padNull (fromIntegral $ blockSize ctx)


decipher :: String -> B.ByteString -> B.ByteString
decipher keyStr = withStrict (cbcDecrypt ctx nullIV) . pad
  where
    ctx = cipherInit key :: AES128
    Right key = makeKey (B.toStrict $ pad $ packStr keyStr)
    pad = padNull (fromIntegral $ blockSize ctx)

-- -------------------------------------

padNull :: Int64 -> B.ByteString -> B.ByteString
padNull n bs = B.append bs $ B.take npad $ B.repeat 0x00
  where npad = n - B.length bs `mod` n

unpadNull :: B.ByteString -> B.ByteString
unpadNull = B.reverse . B.dropWhile (== 0x00) . B.reverse

-- -------------------------------------

withStrict :: (BS.ByteString -> BS.ByteString) -> B.ByteString -> B.ByteString
withStrict fn = B.fromStrict . fn . B.toStrict

-- -----------------------------------------------------------------------------
{-
packStr' :: String -> B.ByteString
packStr' = B.pack . map (fromIntegral.ord)
unpackStr' :: B.ByteString -> String
unpackStr' = map (chr.fromIntegral) . B.unpack
-}
packStr :: String -> B.ByteString
packStr = B.fromStrict . encodeUtf8 . T.pack
unpackStr :: B.ByteString -> String
unpackStr = T.unpack . decodeUtf8 . B.toStrict


-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
