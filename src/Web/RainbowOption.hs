{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module RainbowOption where

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

data RainbowOption = RO {
      underlying1 :: Underlying
    , vol1        :: Double
    , vol2        :: Double
    , underlying2 :: Underlying
    , strike      :: Double
    , rate        :: Double
    , startDate   :: Day
    , endDate     :: Day
} deriving (Show, Generic, Typeable)

pConf = DataConf { monteCarloIter = 4000000 }

instance PricerInput RainbowOption where
    makeInput od = do
      -- TODO: ignoring correlations for now.
      (rawQuotes1, _) <- getRawData (underlying1 od) (startDate od) (endDate od)
      (rawQuotes2, _) <- getRawData (underlying2 od) (startDate od) (endDate od)
      return ([( ConstDisc $ rate od
               , [ BS (underlying1 od) [(day2ContrDate $ startDate od, vol1 od, drift1)]
                 , BS (underlying2 od) [(day2ContrDate $ startDate od, vol2 od, drift2)]]
               , toMarketData (rawQuotes1 ++ rawQuotes2, []) )]
             , mContr)
        where
          drift1 = ((rate od) - ((vol1 od)^2)/2) * years
          drift2 = ((rate od) - ((vol2 od)^2)/2) * years
          years = (fromIntegral (diffDays (endDate od) (startDate od) ) / 365)
          mContr = (day2ContrDate $ startDate od, makeContract od)

makeContract optData =
    let
        und1 = obs (underlying1 optData, 0)
        und2 = obs (underlying2 optData, 0) 
        maturity = fromIntegral $ diffDays (endDate optData) (startDate optData) 
        strk = r $ strike optData
        val = maxx 0 ((maxx und1 und2) - strk)
    in
      transl maturity $ scale val $ transfOne EUR "me" "you"
