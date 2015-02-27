{-# LANGUAGE OverloadedStrings #-}
module Service where

import View
import Pricing (runPricing)
import DataProviders.Data
import CodeGen.DataGen
import Contract
import TypeClass
import Data

import Web.Scotty hiding (body, params)
import Network.Wai.Middleware.Static
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..))
import Control.Monad.Trans
import qualified Data.Map as M

defaultService allContracts availableUnderlyings storedQuotes = do
    get "/" $ homeView allContracts
    get (capture $ contractsBaseUrl ++ ":type") $ do
                       ty <- param "type"
                       contractView allContracts (toMap allContracts M.! ty)
    get "/marketData/underlyings/" $ do
      availUnd <- liftIO availableUnderlyings
      json availUnd
    get "/marketData/view/" $ do 
      quotes <- liftIO storedQuotes
      marketDataView quotes
    middleware $ staticPolicy (addBase "src/Web/static")

api contractType pConf inputData = 
    post (capture ("/api/" ++ contractType)) $
         do
           d <- inputData
           (inp, contr) <- liftIO $ makeInput d
           [res] <- liftIO $ runPricing pConf inp contr
           json $ object ["price" .= res]

toMap = M.fromList . map (\c -> (url c, c))
