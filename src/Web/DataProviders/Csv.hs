module DataProviders.Csv where

import CodeGen.DataGen
import Contract.Date
import DataProviders.Data
import Utils

import Data.Time
import Data.Csv
import Data.List
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

instance FromField Day where
    parseField s = return $ parseDate $ B.unpack s

quotesFile = "./src/Web/sampledata/Quotes.csv"
corrsFile = "./src/Web/sampledata/Correlations.csv"

getRawData :: String -> Day -> Day -> IO ([RawQuotes],[RawCorr])
getRawData und fromD toD = 
    do
      quotes <- getStoredQuotes
      corrs <- getStoredCorrs
      return $ (filterQuotes und fromD toD quotes, corrs)

filterQuotes :: String -> Day -> Day -> [RawQuotes] -> [RawQuotes]
filterQuotes und fromD toD qs = [q | q@(und_, d, p) <- qs, und_ == und && fromD >= d && d <= toD ]

availableUnderlyings :: IO [String]
availableUnderlyings = do
  rawQuotes <- getStoredQuotes
  return $ nub [ und | (und, _, _) <- rawQuotes]

getStoredQuotes :: IO [RawQuotes]
getStoredQuotes = do
  csvQuotes <- BL.readFile quotesFile
  let quotes = case decode NoHeader csvQuotes of
                     Left err -> error err
                     Right v -> V.toList v
  return quotes

getStoredCorrs :: IO [RawCorr]
getStoredCorrs = do
  csvCorrs <- BL.readFile corrsFile
  let corrs = case decode NoHeader csvCorrs of
                     Left err -> error err
                     Right v -> V.toList v
  return corrs

