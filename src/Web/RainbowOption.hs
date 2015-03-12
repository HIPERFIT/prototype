{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module RainbowOption (RainbowOption, rainbowOption, makeContract) where

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

data RainbowOption = RO {
      underlying1 :: Underlying
    , underlying2 :: Underlying
    , strike      :: Double
    , endDate     :: Day
} deriving (Show, Generic, Typeable)

rainbowOption = GUIRepr { guiLabel = "Rainbow option"
                        , params = gtoForm (Proxy :: Proxy (Rep RainbowOption))
                        , url = "rainbowOption" }

makeContract startDate optData =
    let
        und1 = obs (underlying1 optData, 0)
        und2 = obs (underlying2 optData, 0) 
        maturity = fromIntegral $ diffDays (endDate optData) startDate 
        strk = r $ strike optData
        val = maxx 0 ((maxx und1 und2) - strk)
    in
      transl maturity $ scale val $ transfOne EUR "me" "you"
