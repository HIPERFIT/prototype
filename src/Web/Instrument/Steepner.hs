{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Instrument.Steepner (Steepner, steepner, makeContract) where

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
data Steepner = STP {
    frequency        :: Int -- frquency of payments in working days
    , constPay       :: Double
    , margin         :: Double
    , leverage       :: Double
    , shortRate      :: Underlying
    , longRate       :: Underlying
    , endDateConst   :: Day
    , endDateComplex :: Day
} deriving (Show, Generic, Typeable)

steepner = 
    GUIRepr { guiLabel   = "Interest Rate Curve Steepner"
            , formFields = gtoForm (Proxy :: Proxy (Rep Steepner))
            , url        = "steepner" }


iC = chosenBy ("IC", 0) -- Issuer Decision to call

makeContract startDate optData =
    let fv = r 100
        lev    = leverage optData
        longIR = obs (longRate optData, 0)
        shortIR = obs (shortRate optData, 0)
        maturityConst = fromIntegral $ diffDays (endDateConst optData) startDate
        maturityComplex = fromIntegral $ diffDays (endDateComplex optData) (endDateConst optData)
        constPayment = constPayments  (r (constPay optData)) (frequency optData) maturityConst 
        complexPayment = complexPayments  (r (margin optData)) (r lev) longIR shortIR (frequency optData)  maturityComplex
     in both  constPayment (transl maturityConst complexPayment)
-- some helper functions
daysStep = daysStep' []
daysStep' ds step c to = if (c  < to) then daysStep' (ds ++ [c]) step (c+step) to  else ds
constPayments constPayment frequency maturity  =  foreach ( daysStep frequency frequency maturity )
                                           (scale  constPayment   (transfOne EUR "I" "B") ) 
                                           
complexPayments margin lev longIR shortIR frequency maturity  =  foreach ( daysStep frequency frequency maturity )
                                           (
                                           iff  (translExp (nott (iC )) (-1) )
                                           (scale ( margin + lev * (longIR - shortIR) )   (transfOne EUR "I" "B")) 
                                           zero )

