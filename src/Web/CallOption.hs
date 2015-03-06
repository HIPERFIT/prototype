{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module CallOption (CallOption, callOption, makeContract) where

import Contract hiding (i)
import Contract.Date
import Data.Time
import CodeGen.DataGen hiding (startPrice, startDate, endDate)
import qualified Data.Map as M
import GHC.Generics
import Data.Typeable
import TypeClass
import Utils
import Data
import DataProviders.Csv
import DataProviders.Common
import PersistentData

data CallOption = OD {
      underlying :: Underlying
    , strike     :: Double
    , endDate    :: Day
} deriving (Show, Generic, Typeable)


callOption = GUIRepr { guiLabel = "Call option"
                     , params = gtoForm (Proxy :: Proxy (Rep CallOption))
                     , url = "callOption" }

rate = 0.05 :: Double
volatility = 0.2 :: Double 

{-
instance PricerInput CallOption where
    makeInput od = do
      rawData <- getRawData (underlying od) (startDate od) (endDate od)
      return ([(ConstDisc rate, 
                [blackScholesModel od], 
                toMarketData rawData)], 
              mContr)
        where
          mContr = (day2ContrDate $ startDate od, makeContract od)

blackScholesModel od = BS (underlying od) [(day2ContrDate $ endDate od, volatility, drift)]
    where
      drift = (rate - (volatility^2)/2) * years
      years = (fromIntegral (diffDays (endDate od) (startDate od) ) / 365)

makeContract optData = 
    let
        maturity = fromIntegral $ diffDays (endDate optData) (startDate optData) 
        theobs = obs (underlying optData, 0)
        strk = r $ strike optData
    in transl maturity (scale (maxx (theobs - strk) 0) (transfOne EUR "you" "me"))

instance ToPortfolioItem CallOption where
    toPFItem c = PFItem { pFItemStartDate = startDate c
                        , pFItemContractType = "CallOption"
                        , pFItemNominal = nominal c}
-}

makeContract startDate optData = 
    let
        maturity = fromIntegral $ diffDays (endDate optData) startDate 
        theobs = obs (underlying optData, 0)
        strk = r $ strike optData
    in transl maturity (scale (maxx (theobs - strk) 0) (transfOne EUR "you" "me"))
