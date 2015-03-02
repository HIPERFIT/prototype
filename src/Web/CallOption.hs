{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module CallOption (CallOption, callOption) where

import Contract hiding (i)
import Contract.Date
import Data.Time
import CodeGen.DataGen hiding (startPrice, startDate)
import qualified Data.Map as M
import GHC.Generics
import Data.Typeable
import TypeClass
import Utils
import Data
import DataProviders.Csv
import DataProviders.Common

data CallOption = OD {
      underlying :: Underlying
    , strike     :: Double
    , rate       :: Double
    , volatility :: Double
    , startDate  :: Day
    , endDate    :: Day
} deriving (Show, Generic, Typeable)


callOption = GUIRepr { guiLabel = "Call option"
                     , params = gtoForm (Proxy :: Proxy (Rep CallOption))
                     , url = "callOption" }

instance PricerInput CallOption where
    makeInput od = do
      rawData <- getRawData (underlying od) (startDate od) (endDate od)
      return ([(ConstDisc (rate od), 
                [blackScholesModel od], 
                toMarketData rawData)], 
              mContr)
        where
          mContr = (day2ContrDate $ startDate od, makeContract od)

blackScholesModel od = BS (underlying od) [(day2ContrDate $ endDate od, volatility od, drift)]
    where
      drift = ((rate od) - ((volatility od)^2)/2) * years
      years = (fromIntegral (diffDays (endDate od) (startDate od) ) / 365)

makeContract optData = 
    let
        maturity = fromIntegral $ diffDays (endDate optData) (startDate optData) 
        theobs = obs (underlying optData, 0)
        strk = r $ strike optData
    in transl maturity (scale (maxx (theobs - strk) 0) (transfOne EUR "you" "me"))
