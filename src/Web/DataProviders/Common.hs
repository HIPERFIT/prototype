module DataProviders.Common where

import Data.Time
import Data.List
import Utils
import qualified Data.ByteString.Char8 as B
import DataProviders.Data
import CodeGen.DataGen
import Data.Ord
import Data.List (find, sortBy)

toMarketData :: ([RawQuotes],[RawCorr]) -> MarketData
toMarketData (rawQuotes, rawCorrs) = (map toCorr rawCorrs, makeQuotes rawQuotes)
    where
      toCorr (und1, und2, _, corr) = Corr (und1, und2) corr

makeQuotes :: [RawQuotes] -> [Quotes]
makeQuotes qs = map mkEntry groupedQuotes
    where
      groupedQuotes = groupBy (\(x,_,_) (y,_,_) -> x == y) qs
      mkEntry xs@((und,_,_):_) = Quotes und $ map (\(_, date, price) -> (day2ContrDate date, price)) xs

findClosestValues :: [Day] -> [(String, Day, Double)] -> [(String, Day, Double)]
findClosestValues forDays inData = map (closestValueBefore inData) forDays

-- lookup in past data, find closest to the provided date and
-- return this data with date changed to provided date.
closestValueBefore :: [(String, Day, Double)] -> Day -> (String, Day, Double)
closestValueBefore mds d = case clData of
                            Just (und, date, v) -> (und, d, v)
                            Nothing -> throwErr
    where
      clData = find (\(_,d',_) -> d' <= d) $ reverse $ sortBy (comparing extrDate) mds
      throwErr = error ("No data for date " ++ show d)
      extrDate (_, dt, _) = dt
