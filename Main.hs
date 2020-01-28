{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_HADDOCK prune #-}
-- -----------------------------------------------------------------------------
{- |
Module        : Main
Copyright     : (c) Rodger Allen 2018
License       : BSD3
Stability     : Experimental

-}
-- -----------------------------------------------------------------------------
module Main where

import BMP.Bitmap
import ICO.Icons

import StegPal.Encoder
import StegPal.RGBHist
import StegPal.Magic
import StegPal.Analysis
import StegPal.RGBAlpha

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
--import Data.Binary.Put

import System.Console.CmdArgs
import System.FilePath
import Control.Monad
import System.IO
import System.Exit

import qualified Data.ByteString as BS
import Data.Char
import Control.Exception  (bracket)
import Numeric

-- -------------------------------------

version = "0.2.8"
progName = "stegpal"

-- -----------------------------------------------------------------------------

data Steggy = Inject    { inFile  :: FilePath
                        , outFile :: Maybe FilePath
                        , payload :: Maybe FilePath
                        , key     :: Maybe String
                        , verbose :: Bool
                        }
            | Extract   { inFile  :: FilePath
                        , outFile :: Maybe FilePath
                        , key     :: Maybe String
                        , verbose :: Bool
                        }
            | Analyse   { inFile  :: FilePath
                        , outFile :: Maybe FilePath
                        , block   :: Int
                        , hist    :: Bool
                        , space   :: Bool
                        , ent     :: Bool
                        , showPal :: Bool
                        , alphaCh :: Bool
                        , verbose :: Bool
                        }
            | Alpha     { inFile    :: FilePath
                        , outFile   :: Maybe FilePath
                        , payload   :: Maybe FilePath
                        , key       :: Maybe String
                        , verbose   :: Bool
                        }
            deriving (Data,Typeable,Show,Eq)


inject = Inject
  { inFile  = def   &= typFile      &= argPos 0
  , outFile = def   &= typFile      &= help "output file"
  , payload = def   &= typFile      &= help "injected payload (use \"-\" for STDIN)"
  , key     = def   &= typ "KEY"    &= help "password for AES128"
  , verbose = False                 &= help "Verbose" 
  } &= help "Injection using palette histogram"

extract = Extract
  { inFile  = def   &= typFile      &= argPos 0
  , outFile = def   &= typFile      &= help "output file"
  , key     = def   &= typ "KEY"    &= help "password for AES128"
  , verbose = False                 &= help "Verbose" 
  } &= help "Extraction using palette histogram"

analyse = Analyse
  { inFile  = def   &= typFile      &= argPos 0
  , outFile = def   &= typFile      &= help "output file"
  , block   = 32    &= typ "INT"    &= help "block size (default: 32)"  &= name "blocksize"
  , showPal = False                 &= help "create bmp palette"  &= name "p"
  , hist    = False                 &= help "show histogram"      &= name "H"
  , space   = False                 &= help "show palette space"  &= name "s"
  , ent     = False                 &= help "show entropy"        &= name "e"
  , alphaCh = False                 &= help "show alpha space"    &= name "alpha"
  , verbose = False                 &= help "Verbose" 
  } &= help ("Various analysis options\n" ++
            "with no options, defaults to all options")

alpha = Alpha
  { inFile  = def   &= typFile      &= argPos 0
  , outFile = def   &= typFile      &= help "output file"
  , payload = def   &= typFile      &= help "injected payload (use \"-\" for STDIN)"
  , key     = def   &= typ "KEY"    &= help "password for AES128"
  , verbose = False                 &= help "Verbose" 
  } &= help ("Alpha channel injection/extraction\n" ++
            "with payload, it tries to inject; without payload, it tries to extract")

mode :: Mode (CmdArgs Steggy)
mode = cmdArgsMode $ modes [inject,extract,alpha,analyse]
                    &= program progName
                    &= summary (progName ++ " " ++ version)
                    &= help "Bitmap and Icon palette steganography" 

-- -------------------------------------
{-
stegpal 0.2.5

stegpal [COMMAND] ... [OPTIONS]
  bmp and icon palette steganography

Common flags:
  -o --outfile=FILE           output file
  -v --verbose                Verbose
  -? --help                   Display help message
  -V --version                Print version information
     --numeric-version        Print just the version number

stegpal inject [OPTIONS] FILE
  Injection

  -p --payload=FILE           injected payload
  -k --key=KEY                password for AES128

stegpal extract [OPTIONS] FILE
  Extraction

  -k --key=KEY                password for AES128

stegpal analyse [OPTIONS] FILE
  Various analysis options

  -b --blocksize=INT --block  block size
  -g --hist                   show histogram
  -e --ent                    show entropy
  -p --showpal                create bmp palette
-}
-- -----------------------------------------------------------------------------

main = execute =<< cmdArgsRun mode


-- -----------------------------------------------------------------------------
execute opts@(Inject {}) = do
  let dbg       = verbose opts
  let file      = inFile opts
  let stegFile  = maybe (stegName file) id (outFile opts)
  let keyStr    = key opts

  dat <- case (payload opts) of
          Nothing         -> B.getContents
          (Just "-")      -> B.getContents
          (Just datFile)  -> B.readFile datFile

  putDebugLn dbg $ "-- inject"
  putDebugLn dbg $ "File:     " ++ file
  putDebugLn dbg $ "outFile:  " ++ stegFile
  putDebugLn dbg $ "payload:  " ++ maybe "<stdin>" id (payload opts)
  putDebugLn dbg $ "key:      " ++ maybe "" mask (key opts)

  let injected = packerM (key opts) dat

  let zLen = fromIntegral $ B.length injected
  putDebugLn dbg $ "compress: " ++ (show $ zLen) ++ "/" ++ (show $ B.length dat)

  f <- B.readFile file

  let img = runGet get f :: Image
  putDebugLn dbg $ "imgType:  " ++ (show img)
  let space = histSpace img
  putDebugLn dbg $ "space:    " ++ (show $ space)
  when (zLen > space)
       (die $ "Error: too much data to inject: " ++ (show $ zLen) ++ " bytes")
  
  encodeFile stegFile (histInject img injected)

  return ()


-- -----------------------------------------------------------------------------
execute opts@(Extract {}) = do
  let dbg       = verbose opts
  let file      = inFile opts

  putDebugLn dbg $ "-- extract"
  putDebugLn dbg $ "File:     " ++ file
  putDebugLn dbg $ "outFile:  " ++ maybe "<stdout>" id (outFile opts)
  putDebugLn dbg $ "key:      " ++ maybe "" mask (key opts)

  f <- B.readFile file
  putDebugLn dbg $ "imgType:  " ++ (show (runGet get f :: Image))

  let extracted = unpackerM (key opts) (histExtract (runGet get f :: Image))
  maybe (B.putStr extracted) (flip B.writeFile extracted) (outFile opts)

  return ()


-- -----------------------------------------------------------------------------
execute opts@(Alpha {}) = do
  let dbg       = verbose opts
  let file      = inFile opts
  let stegFile  = maybe (alphaName file) id (outFile opts)

  putDebugLn dbg $ "-- alpha"
  putDebugLn dbg $ "File:     " ++ file
  putDebugLn dbg $ "outFile:  " ++ stegFile
  putDebugLn dbg $ "key:      " ++ maybe "" mask (key opts)
  putDebugLn dbg $ "payload:  " ++ maybe "<stdin>" id (payload opts)

  f <- B.readFile file

  -- This is too ugly!
  maybe
    (do
      let extracted = unpackerM (key opts) (alphaExtract (runGet get f :: Image))
      maybe (B.putStr extracted) (flip B.writeFile extracted) (outFile opts)
    )
    (
    \datFile -> do
      dat <- case datFile of
              "-" -> B.getContents
              _   -> B.readFile datFile
      let injected = packerM (key opts) dat
      let zLen = fromIntegral $ B.length injected
      putDebugLn dbg $ "compress: " ++ (show $ zLen) ++ "/" ++ (show $ B.length dat)
      let img = runGet get f :: Image
      putDebugLn dbg $ "imgType:  " ++ (show img)
      let space = alphaSpace img
      putDebugLn dbg $ "space:    " ++ (show $ space)
      when (zLen > space)
        (die $ "Error: too much data to inject: " ++ (show $ zLen) ++ " bytes")
      encodeFile stegFile (alphaInject img injected)
    )
    (payload opts)

  return ()


-- -----------------------------------------------------------------------------
execute opts@(Analyse {}) = do
  let dbg       = verbose opts
  let file      = inFile opts
  let blockSize = block opts
  let palFile   = maybe (palName file) id (outFile opts)

  putDebugLn dbg $ "-- analysis"
  putDebugLn dbg $ "File:     " ++ file
  putDebugLn dbg $ "hist:     " ++ show (hist opts)
  putDebugLn dbg $ "space:    " ++ show (space opts)
  putDebugLn dbg $ "ent:      " ++ show (ent opts)
  putDebugLn dbg $ "showpal:  " ++ show (showPal opts)
  when (showPal opts) $ putDebugLn dbg $ "palFile:  " ++ palFile
  when (showPal opts) $ putDebugLn dbg $ "Blocksize " ++ show blockSize

  f <- B.readFile file
  putDebugLn dbg $ "imgType:  " ++ (show (runGet get f :: Image))

  when (ent opts) $
        showEntropy f
  when (space opts) $
        putStrLn $ "space:    " ++ (show $ histSpace (runGet get f :: Image)) ++ " bytes"
  when (hist opts) $
        showHist $ runGet get f
  when (showPal opts) $
        writePalette blockSize palFile $ runGet get f
  when (not (hist opts) && not (ent opts) && not (showPal opts)) $ do
        showEntropy f
        showHist $ runGet get f
        writePalette blockSize palFile $ runGet get f

  return ()

-- -------------------------------------

showHist :: Image -> IO ()

showHist (ImgBMP img) = do
  putStrLn $ "space:    " ++ (show $ histSpace img) ++ " bytes"
  putStrLn $ show $ histogram img

showHist (ImgICO img) = do
  putStrLn $ "space:    " ++ (show $ histSpace img) ++ " bytes"
  mapM_ (putStrLn.show.histogram) $ iconImgs img

showHist _ = do
  print "unknown (or unhandled) image type"

-- -------------------------------------

showEntropy :: B.ByteString -> IO ()
showEntropy f = do
  let ef = entropy f
  let eb = entropyBin f
  let ep = palEntropy (decode f :: Image)
  let epb = palEntropyBin (decode f :: Image)

  putStrLn $ "entropy-file: " ++ (showFloat6 ef) ++ " / " ++ (showFloat6 eb)
  putStrLn $ "entropy-pal:  " ++ (showFloat6 ep) ++ " / " ++ (showFloat6 epb)

showFloat6 :: RealFloat a => a -> String
showFloat6 n = showFFloatAlt (Just 6) n ""

-- -------------------------------------

writePalette :: Int -> FilePath -> Image -> IO ()

writePalette blockSize file (ImgBMP img) = do
    if (checkCandidateDIB $ bmpDIBHeader img)
    then writeBMP file (paletteImg blockSize (bmpPalette img))
    else putStderrLn $ "Unhandled palette size " ++ show bitCount
  where
    bitCount = dibBitCount $ bmpDIBHeader img

writePalette blockSize file (ImgICO icon) =
    if (not $ null pals)
    then mapM_ (uncurry writeBMP) pals
    else putStderrLn $ "No suitable palettes in icon"
  where
    imgs  = iconImgs icon
    pals  = map (\x -> (palNameIco (show (x+1)) file,
                        paletteImg blockSize (icColors $ imgs!!x))) (idx imgs)
    idx :: (Enum b, Num b) => [IconImage] -> [b]
    idx   = filt . zipWith (\x y -> (x, (icHeader y))) [0..]
    filt :: [(b, DIBHeader)] -> [b]
    filt  = map fst . filter (checkCandidateDIB . snd)

writePalette _ _ _ = print "unknown (or unhandled) image type"

-- -----------------------------------------------------------------------------
-- Some helpers

newName :: FilePath -> FilePath -> [Char]
newName pfx f = (dropExtension f) ++ pfx <.> (takeExtension f)

palName   = newName "-pal"
stegName  = newName "-steg"
alphaName = newName "-alpha"

palNameIco pfx f = (dropExtension f) ++ pfx <.> "bmp"

mask :: String -> String
mask = map (\_ -> '*')
--mask k = take (length k) $ repeat '*'
--mask = flip take (repeat '*') . length

-- -------------------------------------

putDebugLn :: Bool -> String -> IO ()
putDebugLn dbg = when (dbg==True) . hPutStrLn stderr

putStderrLn :: String -> IO ()
putStderrLn = hPutStrLn stderr


-- -----------------------------------------------------------------------------
-- vi: et ts=2 ai
