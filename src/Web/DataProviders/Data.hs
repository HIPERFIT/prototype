module DataProviders.Data where

import Data.Time

type RawQuotes = (String, Day, Double)
type RawCorr = (String, String, Double)
type RawModelData = (String, Day, Double)

data DataProvider = DataProvider { provideQuotes     :: [Day] -> String -> IO [RawQuotes]
                                 , provideCorrs      :: Day   -> String -> IO [RawCorr]
                                 , provideModelData  :: [Day] -> String -> IO [RawModelData]
                                 , storedQuotes      :: IO [RawQuotes]
                                 , storedCorrs       :: IO [RawCorr]
                                 , storedModelData   :: IO [RawModelData]
                                 , storedUnderlyings :: IO [String] }
