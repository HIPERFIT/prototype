{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module CallOption (CallOption, callOption, makeContract) where

import Contract hiding (i)
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

data CallOption = OD {
      underlying :: Underlying
    , strike     :: Double
    , endDate    :: Day
} deriving (Show, Generic, Typeable)


callOption = GUIRepr { guiLabel = "Call option"
                     , params = gtoForm (Proxy :: Proxy (Rep CallOption))
                     , url = "callOption" }

makeContract startDate optData = 
    let
        maturity = fromIntegral $ diffDays (endDate optData) startDate 
        theobs = obs (underlying optData, 0)
        strk = r $ strike optData
    in transl maturity (scale (maxx (theobs - strk) 0) (transfOne EUR "you" "me"))
