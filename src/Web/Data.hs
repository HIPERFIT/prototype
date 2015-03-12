{-# LANGUAGE DeriveGeneric #-}
module Data where

import Data.Typeable (TypeRep)
import TypeClass
import Data.Time
import GHC.Generics
import Data.Text (Text)

type Underlying = String

data ContractGUIRepr  =
    GUIRepr { guiLabel :: String
            , params   :: [(String, TypeRep)]
            , url      :: String }

data CommonContractData = CommonContractData { nominal   :: Int
                                             , startDate :: Day }
                        deriving Generic

data PricingForm = PricingForm { currentDate  :: Maybe Day
                               , interestRate :: Double
                               , iterations   :: Int }
                 deriving Generic

data DataForm = DataForm { fUnderlying :: Text
                         , fDate       :: Day
                         , fVal        :: Double }
                deriving Generic
