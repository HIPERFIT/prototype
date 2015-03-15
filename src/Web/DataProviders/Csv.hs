module DataProviders.Csv (csvDataProvider) where

import CodeGen.DataGen
import Contract.Date
import DataProviders.Data
import DataProviders.Common
import Data
import Utils

import Data.Time
import Data.Csv
import Data.List
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import Data.Maybe

instance FromField Day where
    parseField s = return $ parseDate $ B.unpack s

csvDataProvider = DataProvider { provideQuotes     = getRawQuotes
                               , provideCorrs      = getRawCorrs
                               , provideModelData  = getRawModelData
                               , storedQuotes      = getStoredQuotes
                               , storedCorrs       = getStoredCorrs
                               , storedModelData   = getStoredModelData
                               , storedUnderlyings = availableUnderlyings }            

quotesFile = "./src/Web/sampledata/Quotes.csv"
corrsFile = "./src/Web/sampledata/Correlations.csv"
modelDataFile = "./src/Web/sampledata/ModelData.csv"

getRawQuotes :: [Day] -> String -> IO [RawQuotes]
getRawQuotes days und = 
    do
      quotes <- getStoredQuotes
      return $ findClosestValues days $ filterByUnderlying und quotes

filterByUnderlying und xs = filter (\(und',_,_) -> und' == und) xs

getRawCorrs :: Day -> [Underlying] -> IO [RawCorr]
getRawCorrs d unds = do
  corrs <- getStoredCorrs
  let undPairs = [(und1, und2) | und1 <- unds, und2 <- unds, und1 /= und2]
  return $ filter (\(u1, u2, d', v) -> d' <= d && (u1,u2) `elem` undPairs) corrs

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

getStoredModelData :: IO [RawModelData]
getStoredModelData = do
  csvMd <- BL.readFile modelDataFile
  let md = case decode NoHeader csvMd  of
             Left err -> error err
             Right v -> V.toList v
  return md

getRawModelData :: [Day] -> String -> IO [RawModelData]
getRawModelData days und = do
  md <- getStoredModelData
  return $ findClosestValues days $ filterByUnderlying und md
