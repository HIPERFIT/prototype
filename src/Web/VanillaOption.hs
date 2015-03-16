{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module VanillaOption (VanillaOption, vanillaOption, makeContract) where

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

data VanillaOption = VOpt {
      underlying :: Underlying
    , strike     :: Double
    , endDate    :: Day
    , putOption  :: Bool
} deriving (Show, Generic, Typeable)


vanillaOption = GUIRepr { guiLabel = "Vanilla option"
                        , params = gtoForm (Proxy :: Proxy (Rep VanillaOption))
                        , url = "vanillaOption" }

makeContract startDate optData = 
    let
        maturity = fromIntegral $ diffDays (endDate optData) startDate 
        theobs = obs (underlying optData, 0)
        strk = r $ strike optData
        val = if (putOption optData) then (strk - theobs) else (theobs - strk)
    in transl maturity (scale (maxx val 0) (transfOne EUR "you" "me"))
