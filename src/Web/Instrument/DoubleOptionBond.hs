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
    , frequency   :: Double -- How many times a year there is payment
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


iC = chosenBy ("IC", 0) -- The information about the Investor Decision is a global variablt
bC = chosenBy ("BC", 0) -- same for Buyer Decision

makeContract startDate optData =
    let fv = r 100
        ir = obs (rate optData, 0)
        maturity = fromIntegral $ diffDays (endDate optData) startDate 
        in doubleOptionOnBond fv (r (fvic optData)) (r (fvbc optData)) maturity

doubleOptionOnBond fv fvic fvbc maturity =
 --   let fv = 100 -- Face value usually 100
 --       fvic = 105 -- Face value if issuer calls - can be time dependedt
 --       fvbc = 95 -- Face value if buyer calls 
   --     obsI = obs ("IC", 0)
   --     obsB = obs ("BC", 0)
   --     maturity = 10
    checkWithin (iC !|! bC)    maturity
      -- if (obs ("IC", 0) !|! obs ("BC", 0) ) then    -- How do I do or operator ? What does $ means, how do I send a function as a parameter, when do I use (function p1 p2) or function p1 p2 
          ( iff iC
             (scale fvic (transfOne NOK "I" "B"))
             (scale fvbc (transfOne NOK "I" "B")) )
         (scale fv (transfOne NOK "I" "B"))
         
-- some helper functions

daysStep = daysStep' []
daysStep' ds step c to = if (c  < to) then daysStep' (ds ++ [c]) step (c+step) to  else ds

bondPayments ir margin frequency maturity  =  foreach ( daysStep frequency frequency maturity )
                                           (
                                           iff  (translExp (nott (iC !|! bC)) (-1) )
                                           (scale ( margin + ir)   (transfOne NOK "I" "B")) -- we need to add here the margin
                                           zero ) 


{-

-- Stop / Step 2

--  This needs to be changed as the function is a function of the frequency (i.e. qarterly, semi annually, etc)

onceAQuarter c = foreach [2,4,6,8,10] c


bondPayments ir margin  = onceAQuarter (   iff  (translExp (nott (iC !|! bC)) (-1) )
                                  (scale ( margin + ir)   (transfOne NOK "I" "B")) -- we need to add here the margin
                                  zero )


makeContract startDate optData =
let ..
 in both (doubleOptionOnBond fv (fvic optData) (fvbc optData) maturity)  bondPayments (ir optData) (margin optData)

doubleOptionBond = both doubleOptionOnBond  bondPayments




makeContract startDate optData =
    let
        und1 = obs (underlying1 optData, 0)
        und2 = obs (underlying2 optData, 0) 
        maturity = fromIntegral $ diffDays (endDate optData) startDate 
        strk = r $ strike optData
        val = maxx 0 ((maxx und1 und2) - strk)
    in
      transl maturity $ scale val $ transfOne EUR "me" "you"


-}
