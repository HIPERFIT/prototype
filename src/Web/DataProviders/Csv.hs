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
import Data.Maybe
import Data.Ord
import Data.List (find, sortBy)

instance FromField Day where
    parseField s = return $ parseDate $ B.unpack s

quotesFile = "./src/Web/sampledata/Quotes.csv"
corrsFile = "./src/Web/sampledata/Correlations.csv"
modelDataFile = "./src/Web/sampledata/ModelData.csv"

-- TODO: think about correlations. We not filtering them here.
-- Maybe better use separate functions for corrs and qoutes. 
{-getRawData :: [String] -> [Day] -> IO ([RawQuotes],[RawCorr])
getRawData unds fromD toD = 
    do
      quotes <- getStoredQuotes
      corrs <- getStoredCorrs
      return $ (filterQuotes unds fromD toD quotes, corrs) -}

getRawQuotes :: [Day] -> String -> IO [RawQuotes]
getRawQuotes days und = 
    do
      quotes <- getStoredQuotes
      return $ findClosestData days $ filterByUnderlying und quotes

filterByUnderlying und xs = filter (\(und',_,_) -> und' == und) xs

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
  return $ findClosestData days $ filterByUnderlying und md

findClosestData forDays inData = map (closestDataBefore inData) forDays

closestDataBefore mds d = case clData of
                            Just (und, date, v) -> (und, d, v)
                            Nothing -> throwErr
    where
      clData = find (\(_,d',_) -> d' <= d) $ reverse $ sortBy (comparing extrDate) mds
      throwErr = error ("No data for date " ++ show d)
      extrDate (_, dt, _) = dt
