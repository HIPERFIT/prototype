{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Instrument.EuroHedgedCommodityBasket (EHCBForm, euroHedgedCB4, makeContract)
where

import Contract.Expr
import Contract.Type
import Contract.Date
import Data.Time
import qualified Data.Map as M
import GHC.Generics
import Data.Typeable
import TypeClass
import Utils
import Data
import DataProviders.Csv
import DataProviders.Common
import PersistentData
import Fields

{-@CODE@-}
data EHCBForm = VOpt {
      principal   :: Double
    , underlying1 :: Underlying
    , underlying2 :: Underlying
    , underlying3 :: Underlying
    , underlying4 :: Underlying
    , strike      :: Double
    , protection  :: Double
    , endDate     :: Day
} deriving (Show, Generic, Typeable)


euroHedgedCB4 = 
    GUIRepr { guiLabel   = "Euro Hedged Commodity Basket"
            , formFields = gtoForm (Proxy :: Proxy (Rep EHCBForm))
            , url        = "EuroHedgedCommodityBasket" }

makeContract startDate optData = 
    let
        princ    = r $ principal optData
        maturity = fromIntegral $ diffDays (endDate optData) startDate
        usdeur   = obs ("USDEUR", 0)
        theobs1 = obs (underlying1 optData, 0) * usdeur
        theobs2 = obs (underlying2 optData, 0) * usdeur
        theobs3 = obs (underlying3 optData, 0) * usdeur
        theobs4 = obs (underlying4 optData, 0) * usdeur
        strk  = r $ strike optData
        protect = r $ protection optData
        basket = 0.25 * ((theobs1  + theobs2 + theobs3 + theobs4) / strk) - (1-protect/strk)
        in transl maturity (scale princ (scale  (maxx protect basket) 
                              (transfOne EUR "you" "me")))
