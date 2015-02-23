module DataProviders.Common where

import Data.Time
import Data.List
import Utils
import qualified Data.ByteString.Char8 as B
import DataProviders.Data
import CodeGen.DataGen

toMarketData :: ([RawQuotes],[RawCorr]) -> MarketData
toMarketData (rawQuotes, rawCorrs) = (map toCorr rawCorrs, makeQuotes rawQuotes)
    where
      toCorr (und1, und2, corr) = Corr (und1, und2) corr

makeQuotes :: [RawQuotes] -> [Quotes]
makeQuotes qs = map mkEntry groupedQuotes
    where
      groupedQuotes = groupBy (\(x,_,_) (y,_,_) -> x == y) qs
      mkEntry xs@((und,_,_):_) = Quotes und $ map (\(_, date, price) -> (day2ContrDate date, price)) xs
