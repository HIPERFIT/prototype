{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified CallOption as CO
import qualified RainbowOption as RO
import Pricing
import DataProviders.Csv 
import Data
import View
import Service
import TypeClass
import CodeGen.DataGen
import Utils
import PersistentData
import DB
import Serialization

import Data.Time
import Web.Scotty hiding (body, params)
import Data.Aeson (FromJSON)
import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as PS
import Control.Monad.Trans
import Database.Persist.Sql (fromSqlKey)

instance FromJSON CO.CallOption
instance FromJSON RO.RainbowOption

allContracts = [CO.callOption, RO.rainbowOption]

{-examplePortfolio = [ PFItem {nominal = 10000, contractType = "Call Option", cStartDate = parseDate "2015-01-01"}
                   , PFItem {nominal = 1000, contractType = "Put Option", cStartDate = parseDate "2015-02-01"}
                   , PFItem {nominal = 5000, contractType = "Rainbow Option", cStartDate = parseDate "2015-03-01"}
                   ] -}

exampleModelData = [ ("BASF", 0.2)
                   , ("Siemens", 0.15) ]

main = do
  runDb $ PS.runMigration migrateTables
  scotty 3000 $ do
    api "callOption"    (jsonContract :: ActionM CO.CallOption) CO.makeContract
    --api "rainbowOption" (jsonContract :: ActionM RO.RainbowOption)
    get "/portfolio/" $ do
                  pItems <- liftIO ((runDb $ PS.selectList [] []) :: IO [PS.Entity PFItem])
                  portfolioView $ map fromEntity pItems
    get "/modelData/" $ modelDataView exampleModelData
    defaultService allContracts availableUnderlyings getStoredQuotes

fromEntity p = (show $ fromSqlKey $ P.entityKey p, P.entityVal p)
