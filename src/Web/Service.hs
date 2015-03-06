{-# LANGUAGE GADTs, DeriveGeneric, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Service where

import View
import Pricing (runPricing)
import DataProviders.Data
import DataProviders.Csv
import DataProviders.Common
import CodeGen.DataGen hiding (startDate)
import Contract
import Contract.Expr
import TypeClass
import Data
import PersistentData
import DB
import Serialization
import Utils

import Web.Scotty hiding (body, params)
import Web.Scotty.Internal.Types
import Network.Wai.Middleware.Static
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..))
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as W
import Data.Word
import GHC.Generics
import Data.Data
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey)

instance FromJSON CommonContractData
instance FromJSON PricingForm

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
    post "/pricer/" $ do
      pricingForm <- (jsonParam "conf") :: ActionM PricingForm
      pItems <- liftIO ((runDb $ P.selectList [] []) :: IO [P.Entity PFItem])
      res <- liftIO $ mapM (valuate pricingForm) $ map P.entityVal pItems
      json $ object [ "prices" .= res
                    , "total" .= sum res ]
    delete "/portfolio/:id" $ do
      pfiId <- param "id"
      let key = toSqlKey (fromIntegral ((read pfiId) :: Integer)) :: P.Key PFItem
      liftIO $ runDb $ P.delete key
      text "OK"
      
    middleware $ staticPolicy (addBase "src/Web/static")

api contractType inputData mkContr = 
    post (literal ("/api/" ++ contractType)) $
         do
           commonData <- (jsonParam "common" :: ActionM CommonContractData)
           contractData <- inputData 
           pItems <- liftIO $ runDb $ P.insert $ toPFItem commonData contractData $ mkContr (startDate commonData) contractData
           json $ object ["msg" .= ("Contract added successfully" :: String)]

toMap = M.fromList . map (\c -> (url c, c))

-- Parse contents of parameter p as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonParam p = do
  b <- param p
  either (\e -> raise $ stringError $ "jsonData - no parse: " ++ e ++ ". Data was:" ++ BL.unpack b) return $ eitherDecode b

jsonContract :: (FromJSON a) => ActionM a
jsonContract = jsonParam ("contractData" :: TL.Text)

toPFItem commonData cInput cs = PFItem { pFItemStartDate = startDate commonData
                                       , pFItemContractType = TL.toStrict $ TL.pack $ show $ typeOf cInput
                                       , pFItemNominal = nominal commonData
                                       , pFItemContractSpec = T.pack $ show cs }

makeInput :: PFItem -> PricingForm -> IO ((DiscModel, [Model], MarketData), MContract)
makeInput portfItem pricingForm  = do
  rawModelData <- mapM (\und -> getRawModelData und sDate (contrDate2Day eDate)) unds
  rawMarketData <- getRawData unds sDate (contrDate2Day eDate)
  return ( (ConstDisc $ interestRate pricingForm
           , map toBS $ zip unds $ map (map convertDate) rawModelData
           , toMarketData rawMarketData)
         , (day2ContrDate sDate, contr) )
    where
      toBS (und, md) = bsRiskFreeRate und md (interestRate pricingForm) (day2ContrDate sDate) eDate
      sDate = pFItemStartDate portfItem
      contr = (read $ T.unpack $ pFItemContractSpec portfItem) :: Contract
      cMeta = extractMeta (day2ContrDate sDate, contr)
      eDate = endDate cMeta
      unds  = underlyings cMeta
      convertDate (u, d, v) = (u, day2ContrDate d, v)

valuate pricingForm portfItem = do
  (inp, contr) <- makeInput portfItem pricingForm
  let iter = DataConf { monteCarloIter =  (iterations pricingForm) }
      nominal_ = (fromIntegral (pFItemNominal portfItem))
  [val] <- runPricing iter [inp] contr
  return $  nominal_ * val
