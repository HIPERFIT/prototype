module DataProviders.Data where

import Data.Time
import Data

type RawQuotes = (Underlying, Day, Double)
type RawQuotesExt = (Underlying, Day, Double, Double, Double, Double)
type RawVolatility = (Underlying, Day, Double)
type RawVolatilityMid = (Underlying, Day, Double, Int)
type RawVolatilityModel = (String, Int, Int, Day, Day)
type RawCorr = (Underlying, Underlying, Day, Double)
type RawModelData = (Underlying, Day, Double)

data DataProvider = DataProvider { provideQuotes     :: [Day] ->  Underlying  -> IO [RawQuotes]
                                 , provideCorrs      ::  Day  -> [Underlying] -> IO [RawCorr]
                                 , provideModelData  :: [Day] ->  Underlying  -> IO [RawModelData]
                                 , storedQuotes      :: IO [RawQuotes]
                                 , storedCorrs       :: IO [RawCorr]
                                 , storedModelData   :: IO [RawModelData]
                                 , storedUnderlyings :: IO [Underlying] }
                                    