{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Service where

import View
import Pricing (runPricing)
import DataProviders.Data
import CodeGen.DataGen
import Contract
import TypeClass
import Data

import Web.Scotty hiding (body, params)
import Web.Scotty.Internal.Types
import Network.Wai.Middleware.Static
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..))
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics
import qualified Data.Text.Lazy as TL

instance FromJSON DataConf
deriving instance Generic DataConf 

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

api contractType inputData = 
    post (capture ("/api/" ++ contractType)) $
         do
           conf <- (jsonParam "conf" :: ActionM DataConf)
           contractData <- inputData 
           d <- inputData
           (inp, contr) <- liftIO $ makeInput d
           [res] <- liftIO $ runPricing conf inp contr
           json $ object ["price" .= res]

toMap = M.fromList . map (\c -> (url c, c))

-- Parse contents of parameter p as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonParam p = do
  b <- param p
  either (\e -> raise $ stringError $ "jsonData - no parse: " ++ e ++ ". Data was:" ++ BL.unpack b) return $ eitherDecode b

jsonContract :: (FromJSON a) => ActionM a
jsonContract = jsonParam ("contractData" :: TL.Text)
