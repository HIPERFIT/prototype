{-# LANGUAGE OverloadedStrings #-}
module Main where

import CallOption
import Contract hiding (i)
import Pricing
import Utils
import DataProviders.Csv
import DataProviders.Common
import DataProviders.Data
import Data
import View

import Data.Time
import Web.Scotty hiding (body, params)
import Network.Wai.Middleware.Static
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..))
import Data.Text.Lazy (toStrict)
import Data.Monoid (mconcat, mempty)
import Control.Monad.Trans
import Control.Monad (forM_)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import GHC.Generics (Rep)
import Data.Proxy

instance FromJSON OptionData

price :: MContract -> OptionData -> IO Double
price contr optData = 
    do
      rawMarketData <- getRawData "UNDERLYING" (startDate optData) (endDate optData)
      [price] <- runPricing pConf [(discModel, modelData, toMarketData rawMarketData)] contr
      return price
    where
      (discModel, modelData) = makeInput optData

callOption = GUIRepr { guiLabel = "Call option", params = Proxy :: Proxy (Rep OptionData), url = "/" }

allContracts = [callOption]

main = scotty 3000 $ do
    get (capture $ url callOption) $ homeView allContracts callOption
    service

service = do
    get "/marketData/underlyings/" $ do
                       availUnd <- liftIO availableUnderlyings
                       json availUnd
    get "/marketData/view/" $ do 
                       quotes <- liftIO getStoredQuotes
                       marketDataView quotes
    post "/api/" $ do
      optData <- jsonData :: ActionM OptionData
      res <- liftIO $ price (day2ContrDate $ startDate optData, makeContract optData) optData
      json $ object ["price" .= res]
    middleware $ staticPolicy (addBase "src/Web/static")
