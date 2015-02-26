{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module CallOption where

import Contract hiding (i)
import Contract.Date
import Data.Time
import CodeGen.DataGen hiding (startPrice, startDate)
import qualified Data.Map as M
import GHC.Generics
import Data.Typeable
import Types
import Utils
import Data
import DataProviders.Csv
import DataProviders.Common

data OptionData = OD {
      underlying :: Underlying
    , strike     :: Double
    , rate       :: Double
    , vol        :: Double
    , startDate  :: Day
    , endDate    :: Day
} deriving (Show, Generic, Typeable)

pConf = DataConf { monteCarloIter = 4000000 }

instance PricerInput OptionData where
    makeInput od = do
      rawData <- getRawData "UNDERLYING" (startDate od) (endDate od)
      return ([(ConstDisc (rate od), [BS "UNDERLYING" [(day2ContrDate $ startDate od, vol od, drift)]], toMarketData rawData)], mContr)
        where
          drift = ((rate od) - ((vol od)^2)/2) * years
          years = (fromIntegral (diffDays (endDate od) (startDate od) ) / 365)
          mContr = (day2ContrDate $ startDate od, makeContract od)

makeContract optData = 
    let
        maturity = fromIntegral $ diffDays (endDate optData) (startDate optData) 
        theobs = obs ("UNDERLYING", 0)
        strk = r $ strike optData
    in transl maturity (scale (maxx (theobs - strk) 0) (transfOne EUR "you" "me"))
