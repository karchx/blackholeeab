{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Data.Fits ( HeaderDataUnit(..), dimensions, bitpix, axes, Axes, isBitPixFloat, mainData, pixsUnwrapD, pixDimsByCol, parsePix )
import Data.Fits.Read ( readHDUs )
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Statistics.Sample ( range, mean, variance, stdDev, stdErrMean )
import Kafka.Producer
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (encode, ToJSON, FromJSON)
import Data.Array.IArray (Array, array)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

data RawData = RawData
    { rdHeader :: String
    , rdPixels :: V.Vector Double
    } deriving (Show, Generic)
instance ToJSON RawData
instance FromJSON RawData

-- Kafka configuration
producerProps :: ProducerProperties
producerProps = brokersList ["localhost:9092"]
                <> sendTimeout (Timeout 10000)
                <> setCallback (deliveryCallback print)
                <> logLevel KafkaLogDebug

targetTopic :: TopicName
targetTopic = "blackholes-raw"

mkMessage :: Maybe BS.ByteString -> Maybe BS.ByteString -> ProducerRecord
mkMessage k v  = ProducerRecord
    { prTopic = targetTopic
    , prPartition = UnassignedPartition
    , prKey = k
    , prValue = v
    , prHeaders = mempty
    }

runProducerRaw :: RawData -> IO ()
runProducerRaw rawJson =
    bracket mkProducer clProducer (runHandler rawJson) >>= print
    where
        mkProducer = newProducer producerProps
        clProducer (Left _) = return ()
        clProducer (Right prod) = closeProducer prod
        runHandler _ (Left err) = return $ Left err
        runHandler val (Right prod) = sendMessages prod val

sendMessages :: KafkaProducer -> RawData -> IO (Either KafkaError ())
sendMessages prod rawJson = do
    putStrLn "[DEBUG] Producer is ready"
    let payload = LBS.toStrict $ encode rawJson
    err1 <- produceMessage prod (mkMessage (Just "zero") (Just payload))
    forM_ err1 print

    return $ Right ()

loadFits :: FilePath -> IO BS.ByteString
loadFits path = BS.readFile path

processHDU :: HeaderDataUnit -> IO ()
processHDU hdu = do
        putStrLn $ "[DEBUG] Format bits: " ++ show bpf ++ "\n"
        let pixCount = sum (pixDimsByCol $ (hdu ^. dimensions . axes))
        pixs <- parsePix pixCount bpf (LBS.fromStrict pd)
        let pxsD = if isBitPixFloat bpf then pixsUnwrapD bpf pixs else []
            pVD  = V.fromList pxsD
        let img = toImageArray pVD
        putStrLn $ "[DEBUG] Image array shape: " ++ show img
        if (length ax == 2) && isBitPixFloat bpf
            then do
                let raw = bitMapProcess ax pVD
                runProducerRaw raw
                print raw
            else
                putStrLn $ "[DEBUG] Skipping non 2D float bitmap HDU.\n"
    where
        pd = hdu ^. mainData
        ax = hdu ^. dimensions . axes
        bpf = hdu ^. dimensions . bitpix

toImageArray :: V.Vector Double -> Array (Int, Int, Int) Double
toImageArray v = array ((0,0,0), (199,199,3)) $ do
   i <- [0..199]
   j <- [0..199]
   k <- [0..2]

   pure ((i,j,k), v V.! (k*200*200 + j*200 + i))


bitMapProcess :: Axes          -- ^ Metadata about the column oriented axes
               -> V.Vector Double -- ^ Data is stored in column-row major order
               -> RawData
bitMapProcess []    _   = RawData ("[ERROR] BitMap processing run with no axes.\n") V.empty
bitMapProcess [_]   _   = RawData ("[ERROR] BitMap processing run with only one axis.\n") V.empty
bitMapProcess (_:_) v =
    RawData
        { rdHeader = "DATARAW"
        , rdPixels = V.fromList
            [ mean v
            , range v
            , variance v
            , stdDev v
            , stdErrMean v
            ]
        }

main :: IO ()
main = do
   fitsData <- loadFits "Spiral_2_30_0_300_10_0_NoGrad.fits"
   case readHDUs fitsData of
         Left err -> putStrLn $ "Error parsing FITS file: " ++ err
         Right hdus -> do
            putStrLn $ "HDU Total " ++ show (length hdus) ++ " HDUs \n"
            mapM_ processHDU hdus
