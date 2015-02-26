{-# LANGUAGE OverloadedStrings #-}
module Service where

import View

import Web.Scotty hiding (body, params)
import Network.Wai.Middleware.Static
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..))
import Control.Monad.Trans
import Pricing (runPricing)
import DataProviders.Data
import CodeGen.DataGen
import Contract

defaultService inputData availableUnderlyings storedQuotes pConf = do
    get "/marketData/underlyings/" $ do
      availUnd <- liftIO availableUnderlyings
      json availUnd
    get "/marketData/view/" $ do 
      quotes <- liftIO storedQuotes
      marketDataView quotes
    post "/api/:type" $ do
      (inp, contr) <- inputData
      [res] <- liftIO $ runPricing pConf inp contr -- $ price (day2ContrDate $ startDate optData, makeContract optData) optData
      json $ object ["price" .= res]
    middleware $ staticPolicy (addBase "src/Web/static")
