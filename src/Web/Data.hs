{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Data where

import Data.Typeable (TypeRep, Typeable)
import TypeClass
import Text.Blaze.Html5 (Html)
import Data.Time
import GHC.Generics
import Data.Text (Text)

type Underlying = String

data VolatilityModel = String

data PercentField = PercentField Double
                  deriving (Show, Generic, Typeable)
fromPercentField (PercentField a) = a / 100

data ContractGUIRepr  =
    GUIRepr { guiLabel   :: String
            , formFields :: [(String, FormField Html)]
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
                
data DataFormVolatility = DataFormVolatility { vUnderlying :: Text
                                             , vstartDate  :: Day
                                             , volVal      :: Double 
                                             , vperiod     :: Int
                                             , vmodel      :: String}
                 deriving Generic

                

data CorrForm = CorrForm { corrUnd1 :: Text
                         , corrUnd2 :: Text
                         , corrDate :: Day
                         , corrVal  :: Double }
                deriving Generic

data StockGraphForm = StockGraphForm { sUnderlying1 :: Underlying
                                     , sUnderlying2 :: Underlying
                                     , sstartDate :: Day
                                     , sendDate :: Day
                                     , normalize :: Bool}
                deriving Generic
       
data VolatilityGraphForm = VolatilityGraphForm { volUnderlying :: Underlying
                                                          ,volstartDate :: Day
                                                          ,volendDate :: Day}
                deriving Generic         
              

data ContractGraphForm = ContractGraphForm { ccontract :: String
                                           , cstartDate :: Maybe Day
                                           , cendDate :: Maybe Day
                                           , cinterestRate :: PercentField
                                           , citerations   :: Int }
                deriving Generic


