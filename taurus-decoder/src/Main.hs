module Main (main) where

import Data.Fits as FITS
import Data.Fits.Read ( readHDUs )
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import Control.Monad (forM_)

loadFits :: FilePath -> IO BS.ByteString
loadFits path = BS.readFile path

readFits :: BS.ByteString -> IO ()
readFits fitsData = do
    case readHDUs fitsData of
        Left err -> putStrLn $ "Error reading FITS data: " ++ err
        Right hdus -> do
            putStrLn $ "Number of HDUs: " ++ show (length hdus)

main :: IO ()
main = do
   putStrLn "This is a placeholder for FITS file processing."

-- main :: IO ()
-- main = do
--     fitsData <- BS.readFile "WFPC2u5780205r_c0fx.fits"
--     case parseFits fitsData of
--         Left err -> putStrLn $ "Error parsing FITS file: " ++ err
--         Right fits -> do
--             -- Inspect HDUs
--             let hdus = fitsHDUs fits
--             forM_ (zip [0..] hdus) $ \(i, hdu) -> do
--                 putStrLn $ "HDU " ++ show i ++ ":"
--                 putStrLn $ "  Type: " ++ show (hduType hdu)
