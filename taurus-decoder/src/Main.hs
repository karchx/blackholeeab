module Main (main) where

import qualified Data.Fits as FITS
import Data.Fits ( HeaderDataUnit(..), dimensions, bitpix, axes )
import Data.Fits.Read ( readHDUs )
import qualified Data.ByteString as BS

import Lens.Micro ((^.))

loadFits :: FilePath -> IO BS.ByteString
loadFits path = BS.readFile path

processHDU :: HeaderDataUnit -> IO ()
processHDU hdu = do
    let header = hdu ^. FITS.header
        dims = hdu ^. dimensions
        bpf = dims ^. bitpix
        ax = hdu ^. dimensions . axes
    putStrLn $ "Format bits: " ++ show bpf ++ "\n"
    putStrLn $ "Axes: " ++ show ax ++ "\n"
    mapM_ (\a -> putStrLn $ "Axis: " ++ show a ++ "\n") ax

main :: IO ()
main = do
   fitsData <- loadFits "Spiral_2_30_0_300_10_0_NoGrad.fits"
   case readHDUs fitsData of
         Left err -> putStrLn $ "Error parsing FITS file: " ++ err
         Right hdus -> do
            putStrLn $ "HDU Total " ++ show (length hdus) ++ " HDUs \n"
            mapM_ processHDU hdus
