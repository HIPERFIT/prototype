{-# LANGUAGE GADTs, DeriveGeneric, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
module Service where

import View
import Pricing (runPricing)
import DataProviders.Data
import DataProviders.Common
import CodeGen.DataGen hiding (startDate)
import Contract.Expr
import Contract.Type
import Contract.Environment
import Contract.Transform
import Contract.Analysis
import Contract (cashflows)
import TypeClass
import Data
import PersistentData
import DB
import Serialization
import Utils
import CodeGen.Utils
import Auth
import Stocks.FetchStocks

import Web.Scotty hiding (body, params)
import Web.Scotty.Internal.Types hiding (Env)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..), encode, ToJSON (..))
import Control.Monad.Trans
import Control.Monad (when)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as W
import Data.Word
import GHC.Generics
import Data.Data
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Time (Day, diffDays, addDays)
import Data.Text (Text)
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Exit

instance FromJSON CommonContractData
instance FromJSON PricingForm
instance FromJSON DataForm
instance FromJSON CorrForm
instance FromJSON PercentField
instance FromJSON ContractGraphForm
instance ToJSON PercentField

defaultService allContracts dataProvider = do
    get "/" $ basicAuth $ homeView allContracts
    get (capture $ contractsBaseUrl ++ ":type") $ do
                       ty <- param "type"
                       code <- liftIO $ readInstrument $ capFirst ty
                       contractView allContracts (toMap allContracts M.! ty) code
    post "/pricer/" $ basicAuth $ do
      pricingForm <- (jsonParam "conf") :: ActionM PricingForm
      pItems <- liftIO ((runDb $ P.selectList [] []) :: IO [P.Entity PFItem])
      res <- liftIO $ mapM (maybeValuate pricingForm dataProvider) $ map P.entityVal pItems
      json $ object [ "prices" .= res
                    , "total"  .= (sum $ map (fromMaybe 0) res) ]
    get "/portfolio/" $ basicAuth $ do      
      pItems <- liftIO ((runDb $ P.selectList [] []) :: IO [P.Entity PFItem])
      currDate <- liftIO getCurrentTime
      items <- liftIO $ mapM ((withHorAndCashflows dataProvider) . fromEntity) pItems
      let aggrCashflows = sortBy (comparing (\(d,_,_,_,_,_) -> d)) $ concat $ map (\(_,_,_,cf) -> cf) items
      portfolioView items (utctDay currDate) aggrCashflows
                        [ ("currentDate", Just $ show $ utctDay currDate)
                        , ("interestRate", Just "2")
                        , ("iterations", Just "10000")]
    delete "/portfolio/:id" $ basicAuth $ do
      pfiId <- param "id"
      let key = toSqlKey (fromIntegral ((read pfiId) :: Integer)) :: P.Key PFItem
      liftIO $ runDb $ P.delete key
      text "OK"
    get "/marketData/underlyings/" $ basicAuth $ do
      availUnd <- liftIO (storedUnderlyings dataProvider)
      json availUnd
    get "/marketData/view/" $ basicAuth $ do 
      quotes <- liftIO $ storedQuotes dataProvider
      corrs  <- liftIO $ storedCorrs dataProvider
      marketDataView quotes corrs
    post "/marketData/quotes/" $ basicAuth $ do
      form <- jsonData :: ActionM DataForm
      let md = DbQuotes (fUnderlying form) (fDate form) (fVal form) (toSqlKey (fromIntegral defaultUserId))
      liftIO $ runDb $ P.insert_ md
      json $ object ["msg" .= ("Data added successfully" :: String)]
    delete "/marketData/quotes/" $ basicAuth $ do
      key <- jsonData :: ActionM (Text, Day)
      liftIO $ runDb $ P.deleteBy $ (uncurry QuoteEntry) key
      text "OK"
    delete "/marketData/corrs/" $ basicAuth $ do
      (und1, und2, d) <- jsonData :: ActionM (Text, Text, Day)
      liftIO $ runDb $ P.deleteBy $ CorrEntry und1 und2 d
      text "OK"
    post "/marketData/corrs/" $ basicAuth $ do
      form <- jsonData :: ActionM CorrForm
      let corr = DbCorr (corrUnd1 form) (corrUnd2 form) (corrDate form) (corrVal form) (toSqlKey (fromIntegral defaultUserId))
      liftIO $ runDb $ P.insert_ corr
      json $ object ["msg" .= ("Data added successfully" :: String)]
    get "/modelData/" $ basicAuth $ do
      md <- liftIO $ storedModelData dataProvider
      modelDataView md
    post   "/modelData/" $ basicAuth $ do
      form <- jsonData :: ActionM DataForm
      let md = DbModelData (fUnderlying form) (fDate form) (fVal form) (toSqlKey (fromIntegral defaultUserId))
      liftIO $ runDb $ P.insert_ md
      json $ object ["msg" .= ("Data added successfully" :: String)]
    delete "/modelData/" $ basicAuth $ do
      key <- jsonData :: ActionM (Text, Day)
      liftIO $ runDb $ P.deleteBy $ (uncurry MDEntry) key
      text "OK"
    get   "/marketData/stocks/:id" $ basicAuth $ do
      stock_id <- param "id"
      startdate <- param "startdate"
      enddate <- param "enddate"
      stockData <- liftIO $ update_db_quotes stock_id startdate enddate "Yahoo"
      json stockData
    get   "/contractGraph/" $ basicAuth $ do
      contractGraphView
    post  "/contractGraph/contracts/" $ basicAuth $ do
      form <- (jsonParam "conf") :: ActionM ContractGraphForm

      let pfiId = ccontract form
      pItems <- liftIO ((runDb $ P.selectList [] []) :: IO [P.Entity PFItem])
      let pItems2 = map (withHorizon . fromEntity) pItems
      let pfItem_temp = filter (\(x,y,z) -> x==pfiId) pItems2
      let pfItem = (\((x,y,z):xs) -> y) pfItem_temp
      let sDate = pFItemStartDate pfItem
      let mContr = (day2ContrDate sDate, read $ T.unpack $ pFItemContractSpec pfItem)
      let cMeta = extractMeta mContr
      let unds  = underlyings cMeta

      let startdate = daytoString (pFItemStartDate pfItem)
      let enddate = maybeDaytoString (cendDate form)
      a <- liftIO $ mapM (\x -> update_db_quotes x (if startdate < (maybeDaytoString (cstartDate form)) then startdate else maybeDaytoString (cstartDate form)) enddate "Yahoo") unds

      let dates = getAllDays (cstartDate form) (cendDate form)
      res <- liftIO $ mapM (maybeValuateGraph pfItem dataProvider form) dates
      json res
    get   "/contractGraph/listOfContracts/" $ basicAuth $ do
      pItems <- liftIO ((runDb $ P.selectList [] []) :: IO [P.Entity PFItem])
      let pItems2 = map (withHorizon . fromEntity) pItems
      json (map prettifyContract pItems2)
    middleware $ staticPolicy (addBase "src/Web/static")

api contractType inputData mkContr = 
    post (literal ("/api/" ++ contractType)) $
         do
           commonData <- (jsonParam "common" :: ActionM CommonContractData)
           contractData <- inputData 
           pItems <- liftIO $ runDb $ P.insert $ toPFItem commonData contractData $ mkContr (startDate commonData) contractData
           json $ object ["msg" .= ("Contract added successfully" :: String)]

simplWithFixings dataProvider portfItem = do
  currDate <- liftIO getCurrentTime
  quotesBefore <- mapM (getRawQuotes $ sDate : filter (<= utctDay currDate) allDays) unds
  let env = (makeEnv (concat quotesBefore)) $ emptyFrom $ day2ContrDate sDate
  return $ simplify env mContr
    where sDate = pFItemStartDate portfItem
          mZero = (day2ContrDate sDate, zero)
          mContr = (day2ContrDate sDate, read $ T.unpack $ pFItemContractSpec portfItem)
          cMeta = extractMeta mContr
          allDays = map contrDate2Day (allDates cMeta)
          unds  = underlyings cMeta
          getRawQuotes = provideQuotes dataProvider

                
toMap = M.fromList . map (\c -> (url c, c))

-- Parse contents of parameter p as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonParam p = do
  b <- param p
  either (\e -> raise $ stringError $ "jsonData - no parse: " ++ e ++ ". Data was:" ++ BL.unpack b) return $ eitherDecode b

jsonContract :: (FromJSON a) => ActionM a
jsonContract = jsonParam ("contractData" :: TL.Text)

toPFItem commonData cInput cs = PFItem { pFItemStartDate = startDate commonData
                                       , pFItemContractType = TL.toStrict $ TL.pack $ show $ typeOf cInput
                                       , pFItemQuantity = quantity commonData
                                       , pFItemContractSpec = T.pack $ show cs
                                       , pFItemPortfolioId = toSqlKey $ fromIntegral defaultPortfolioId }

-- TODO: Refactoring needed. Possibly, merge with mkData
makeInput :: MContract -> PricingForm -> DataProvider -> IO ((DiscModel, [Model], MarketData), MContract)
makeInput mContr@(sDate, contr) pricingForm dataProvider = do
  (modelData, marketData) <- mkData mContr pricingForm dataProvider
  return ( (ConstDisc $ fromPercentField $ interestRate pricingForm
           , modelData
           , marketData) 
         , mContr)
    where
      currDate = fromMaybe (contrDate2Day sDate) $ currentDate pricingForm 
      dt = fromIntegral $ diffDays currDate $ contrDate2Day sDate
      cMeta = extractMeta mContr
      allDays = map contrDate2Day (allDates cMeta)

mkData mContr@(sDate, contr) pricingForm dataProvider = do
  rawModelData <- mapM (getRawModelData allDays) unds
  rawQuotes <- mapM (getRawQuotes $ (contrDate2Day sDate) : allDays) unds
  rawCorrs <- getRawCorrs currDate unds
  return ( map toBS $ zip unds $ rawModelData
         , toMarketData $ (concat rawQuotes, rawCorrs))
    where
      currDate = fromMaybe (contrDate2Day sDate) $ currentDate pricingForm 
      toBS (und, md) = bsRiskFreeRate und (map convertDate md) (fromPercentField $ interestRate pricingForm) sDate eDate
      cMeta = extractMeta mContr
      eDate = endDate cMeta
      unds  = underlyings cMeta
      allDays = map contrDate2Day (allDates cMeta)
      getRawModelData = provideModelData dataProvider
      getRawQuotes = provideQuotes dataProvider
      getRawCorrs = provideCorrs dataProvider
      convertDate (u, d, v) = (u, day2ContrDate d, v)

makeEnv quotes = foldr (.) id $ map f quotes
    where
      f (und, d, q) = addFixing (und, day2ContrDate d, q)

maybeValuate :: PricingForm -> DataProvider -> PFItem -> IO (Maybe Double)
maybeValuate pricingForm dataProvider portfItem = if (dt >= 0) then
                                                      do v <- valuate pricingForm dataProvider portfItem
                                                         return $ Just v
                                                  else return Nothing
    where
      currDate = fromMaybe (pFItemStartDate portfItem) $ currentDate pricingForm
      dt = fromIntegral $ diffDays currDate $ pFItemStartDate portfItem

valuate pricingForm dataProvider portfItem = do
  quotesBefore <- mapM (getRawQuotes $ sDate : filter (<= currDate) allDays) unds
  let env = (makeEnv (concat quotesBefore)) $ emptyFrom $ day2ContrDate sDate
      simplContr = advance dt $ simplify env mContr
  (inp, contr) <- makeInput simplContr pricingForm dataProvider
  let iter = DataConf { monteCarloIter =  (iterations pricingForm) }
      quantity_ = (fromIntegral (pFItemQuantity portfItem))
  [val] <- runPricing iter [inp] contr
  return $ quantity_ * val
  where
    currDate = fromMaybe sDate $ currentDate pricingForm 
    dt = fromIntegral $ diffDays currDate sDate
    sDate = pFItemStartDate portfItem
    mZero = (day2ContrDate sDate, zero)
    mContr = (day2ContrDate sDate, read $ T.unpack $ pFItemContractSpec portfItem)
    cMeta = extractMeta mContr
    allDays = map contrDate2Day (allDates cMeta)
    unds  = underlyings cMeta
    getRawQuotes = provideQuotes dataProvider

fromEntity p = (show $ fromSqlKey $ P.entityKey p, P.entityVal p)

withHorizon (key, entity) = (key, entity, addDays days $ pFItemStartDate entity)
    where
      days = fromIntegral $ horizon $ read $ T.unpack $ pFItemContractSpec entity

withHorAndCashflows dataProvider (key, entity) = do
  let days = fromIntegral $ horizon $ read $ T.unpack $ pFItemContractSpec entity
  c <- simplWithFixings dataProvider entity
  return (key, entity, addDays days $ pFItemStartDate entity, cashflows $ c)
         
createDefaultUser = createIfNotExist (defaultUserId, User "hiperfit" "123")
  
createDefaultPortfolio = createIfNotExist (defaultPortfolioId, Portfolio "HIPERFIT" (toSqlKey $ fromIntegral defaultUserId))

createIfNotExist (entId, ent) = do
    let key = toSqlKey $ fromIntegral entId
    ent_ <- runDb $ P.get key
    let exist = case ent_ of
                   Just _ -> False
                   Nothing -> True
    when exist $ runDb $ P.insertKey key ent

readInstrument name = do
  allCode <- readFile ("src/Web/Instrument/" ++ name ++ ".hs")
  let [_,res] = T.splitOn "{-@CODE@-}" $ T.pack allCode
  return res

authUser u p | u == "hiperfit" && p == "123" = Authorized
             | otherwise = Unauthorized


maybeDaytoString :: Maybe Day -> String
maybeDaytoString (Just t) = formatTime defaultTimeLocale "%F" t
maybeDaytoString Nothing = error "No date given"

daytoString :: Day -> String
daytoString t = formatTime defaultTimeLocale "%F" t

maybeValuateGraph :: PFItem -> DataProvider -> ContractGraphForm -> Day -> IO (String, Maybe Double)
maybeValuateGraph pfItem dataProvider form date = do
  let pricingForm = PricingForm {currentDate=Just date,interestRate=cinterestRate form,iterations=citerations form}
  res <- (maybeValuate pricingForm dataProvider pfItem)
  return(daytoString date, res)


nextDay :: Day -> Day -> [Day]
nextDay startDate endDate = if (daytoString startDate) == (daytoString endDate) then [startDate] else startDate : (nextDay (addDays 1 startDate) endDate)

getAllDays :: Maybe Day -> Maybe Day -> [Day]
getAllDays Nothing _ = error "Missing starting date"
getAllDays _ Nothing = error "Missing end date"
getAllDays (Just startDate) (Just endDate) = do
  let a = if (daytoString startDate) >= (daytoString endDate) then error "Starting date needs to before end date" else 2
  nextDay startDate endDate


prettifyContract (id,pfItem,endDate) = do
  let sDate = pFItemStartDate pfItem
  (id, (daytoString sDate) ++ " - " ++ (daytoString endDate))
