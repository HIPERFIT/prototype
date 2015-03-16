{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data where

import Data.Typeable (TypeRep, Typeable)
import TypeClass
import Data.Time
import GHC.Generics
import Data.Text (Text)

type Underlying = String

data PercentField = PercentField Double
                  deriving (Show, Generic, Typeable)
fromPercentField (PercentField a) = a / 100

data ContractGUIRepr  =
    GUIRepr { guiLabel   :: String
            , formFields :: [(String, TypeRep)]
            , url        :: String }

data CommonContractData = CommonContractData { nominal   :: Int
                                             , startDate :: Day }
                        deriving Generic

data PricingForm = PricingForm { currentDate  :: Maybe Day
                               , interestRate :: PercentField
                               , iterations   :: Int }
                 deriving Generic

data DataForm = DataForm { fUnderlying :: Text
                         , fDate       :: Day
                         , fVal        :: Double }
                deriving Generic

data CorrForm = CorrForm { corrUnd1 :: Text
                         , corrUnd2 :: Text
                         , corrDate :: Day
                         , corrVal  :: Double }
                deriving Generic
