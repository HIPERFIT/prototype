{-# LANGUAGE DeriveGeneric #-}
module Data where

import Data.Typeable (TypeRep)
import TypeClass
import Data.Time
import GHC.Generics

type Underlying = String

data ContractGUIRepr  =
    GUIRepr { guiLabel :: String
            , params   :: [(String, TypeRep)]
            , url      :: String }

data CommonContractData = CommonContractData { nominal   :: Int
                                             , startDate :: Day }
                        deriving Generic

data PricingForm = PricingForm { currentDate  :: Day
                               , interestRate :: Double
                               , iterations   :: Int }
                 deriving Generic
