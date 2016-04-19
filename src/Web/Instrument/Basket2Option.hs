{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Instrument.Basket2Option (Basket2Option, basket2Option, makeContract)
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
data Basket2Option = VOpt {
      underlying1 :: Underlying
    , strike1     :: Double
    , underlying2 :: Underlying
    , strike2     :: Double
    , protection  :: Double
    , endDate     :: Day
} deriving (Show, Generic, Typeable)


basket2Option = 
    GUIRepr { guiLabel   = "Basket2 option"
            , formFields = gtoForm (Proxy :: Proxy (Rep Basket2Option))
            , url        = "Basket2Option" }

makeContract startDate optData = 
    let
        maturity = fromIntegral $ diffDays (endDate optData) startDate 
        theobs1 = obs (underlying1 optData, 0)
        theobs2 = obs (underlying2 optData, 0)
        strk1 = r $ strike1 optData
        strk2 = r $ strike2 optData
        protect = r $ protection optData
        basket = 0.5 * ((theobs1 / strk1) + (theobs2 / strk2))
    in transl maturity (scale (maxx protect basket) 
                              (transfOne EUR "you" "me"))
