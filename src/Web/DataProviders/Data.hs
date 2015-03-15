module DataProviders.Data where

import Data.Time
import Data

type RawQuotes = (Underlying, Day, Double)
type RawCorr = (Underlying, Underlying, Day, Double)
type RawModelData = (Underlying, Day, Double)

data DataProvider = DataProvider { provideQuotes     :: [Day] ->  Underlying  -> IO [RawQuotes]
                                 , provideCorrs      ::  Day  -> [Underlying] -> IO [RawCorr]
                                 , provideModelData  :: [Day] ->  Underlying  -> IO [RawModelData]
                                 , storedQuotes      :: IO [RawQuotes]
                                 , storedCorrs       :: IO [RawCorr]
                                 , storedModelData   :: IO [RawModelData]
                                 , storedUnderlyings :: IO [Underlying] }
