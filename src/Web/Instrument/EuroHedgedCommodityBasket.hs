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
      nominalPrice :: Double
    , referencePrice :: PercentField
    , underlying1    :: Underlying
    , underlying2    :: Underlying
    , underlying3    :: Underlying
    , underlying4    :: Underlying
    , strike         :: PercentField
    , protection     :: PercentField
    , endDate        :: Day
} deriving (Show, Generic, Typeable)


euroHedgedCB4 = 
    GUIRepr { guiLabel   = "Euro Hedged Commodity Basket"
            , formFields = gtoForm (Proxy :: Proxy (Rep EHCBForm))
            , url        = "EuroHedgedCommodityBasket" }

makeContract startDate optData = 
    let
        np       = r $ nominalPrice optData
        rp       = r $ fromPercentField $referencePrice optData
        maturity = fromIntegral $ diffDays (endDate optData) startDate
        usdeur   = obs ("USDEUR", 0)
        theobs1  = obs (underlying1 optData, 0) / obs (underlying1 optData, -maturity) * usdeur
        -- Observations prices are relative to the price at the beginning of the contract.
        -- We keep this format to avoid let binding and we keep the same for all 4 observations.
        theobs2  = obs (underlying2 optData, 0) / obs (underlying2 optData, -maturity) * usdeur
        theobs3  = obs (underlying3 optData, 0) / obs (underlying3 optData, -maturity) * usdeur
        theobs4  = obs (underlying4 optData, 0) / obs (underlying4 optData, -maturity) * usdeur
        strk     = r $ fromPercentField $ strike optData
        protect  = r $ fromPercentField $ protection optData
        basket   = 0.25 * (rp *  (theobs1  + theobs2 + theobs3 + theobs4) / strk) - (1-protect/strk)
        in transl maturity  (scale  (np * (maxx protect basket))
                              (transfOne EUR "you" "me"))
