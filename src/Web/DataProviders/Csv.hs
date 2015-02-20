module DataProviders.Csv where

import CodeGen.DataGen
import Contract.Date
import Utils

import Data.Time
import Data.List
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

type RawQuotes = (String, Day, Double)
type RawCorr = (String, String, Double)

instance FromField Day where
    parseField s = return $ parseDate $ B.unpack s

quotesFile = "./src/Web/sampledata/Quotes.csv"
corrsFile = "./src/Web/sampledata/Correlations.csv"

toMarketData :: ([RawQuotes],[RawCorr]) -> MarketData
toMarketData (rawQuotes, rawCorrs) = (map toCorr rawCorrs, makeQuotes rawQuotes)
    where
      toCorr (und1, und2, corr) = Corr (und1, und2) corr

getRawData :: String -> Day -> Day -> IO ([RawQuotes],[RawCorr])
getRawData und fromD toD = 
    do
      csvQuotes <- BL.readFile quotesFile
      csvCorrs <- BL.readFile corrsFile
      let quotes = case decode NoHeader csvQuotes of
                     Left err -> error err
                     Right v -> V.toList v
      let corrs = case decode NoHeader csvCorrs of
                    Left err -> error err
                    Right v -> V.toList v
      return $ (filterQuotes und fromD toD quotes, corrs)

filterQuotes :: String -> Day -> Day -> [RawQuotes] -> [RawQuotes]
filterQuotes und fromD toD qs = [q | q@(und_, d, p) <- qs, und_ == und && fromD >= d && d <= toD ]
                                      

makeQuotes :: [RawQuotes] -> [Quotes]
makeQuotes qs = map mkEntry groupedQuotes
    where
      groupedQuotes = groupBy (\(x,_,_) (y,_,_) -> x == y) qs
      mkEntry xs@((und,_,_):_) = Quotes und $ map (\(_, date, price) -> (day2ContrDate date, price)) xs

