{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Instrument.DoubleOptionBond (DoubleOptionBond, doubleOptionBond, makeContract) where

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
data DoubleOptionBond = DOB {
      price       :: Double
    , frequency   :: Int -- frquency of payments in working days
    , fvic        :: Double
    , fvbc        :: Double
    , margin      :: Double
    , rate        :: Underlying
    , endDate     :: Day
} deriving (Show, Generic, Typeable)

doubleOptionBond = 
    GUIRepr { guiLabel   = "Senior Unsecured Double Option Bond"
            , formFields = gtoForm (Proxy :: Proxy (Rep DoubleOptionBond))
            , url        = "doubleOptionBond" }


iC = chosenBy ("IC", 0) --  Issuer Decision to call
bC = chosenBy ("BC", 0) --  Buyer Decision to call

makeContract startDate optData =
    let fv = r 100
        ir = obs (rate optData, 0)
        maturity = fromIntegral $ diffDays (endDate optData) startDate
        bp = bondPayments ir (r (margin optData)) (frequency optData) maturity 
        optionality = doubleOptionOnBond fv (r (fvic optData)) (r (fvbc optData)) maturity
     in both  bp optionality
-- some helper functions
doubleOptionOnBond fv fvic fvbc maturity =
    checkWithin (iC !|! bC)    maturity
          ( iff iC
             (scale fvic (transfOne NOK "I" "B"))
             (scale fvbc (transfOne NOK "I" "B")) )
         (scale fv (transfOne NOK "I" "B"))
daysStep = daysStep' []
daysStep' ds step c to = if (c  < to) then daysStep' (ds ++ [c]) step (c+step) to  else ds
bondPayments ir margin frequency maturity  =  foreach ( daysStep frequency frequency maturity )
                                           (
                                           iff  (translExp (nott (iC !|! bC)) (-1) )
                                           (scale ( margin + ir)   (transfOne NOK "I" "B")) 
                                           zero )

