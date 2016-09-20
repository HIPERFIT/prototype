{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Instrument.VanillaOption as VO
import qualified Instrument.RainbowOption as RO
import qualified Instrument.Basket2Option as BO
import qualified Instrument.DoubleOptionBond as DOB
import qualified DataProviders.Database as DBP
import qualified DataProviders.Csv as CsvP
import DataProviders.Data
import View
import Service
import PersistentData
import DB
import Serialization
import Data
import Utils
import qualified Stocks.FetchStocks as F
import System.Environment

import Data.Time
import Web.Scotty hiding (body, params, options)
import Data.Aeson (FromJSON)
import qualified Database.Persist.Sql as P
import System.Environment (getArgs)
import Control.Monad (when)
import System.Console.GetOpt
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.Persist (insert)
import Data.Csv
import Contract.Date(at)

instance FromJSON VO.VanillaOption
instance FromJSON RO.RainbowOption
instance FromJSON BO.Basket2Option
instance FromJSON DOB.DoubleOptionBond

allContracts = [VO.vanillaOption, RO.rainbowOption, BO.basket2Option, DOB.doubleOptionBond]
defaultPort = 3000
initialSymbols = ["AAPL", "GOOGL", "CAT", "YHOO", "SHLD", "IBM", "MSFT"]

data Flag = Port String | InitData | DataFile String | DelQuotes
      deriving Show
   
options :: [OptDescr Flag]
options =
    [ Option ['i'] ["initdata"] (NoArg InitData)     "Fetch quotes for a number of stocks (e.g., AAPL and GOOGL) from Yahoo"
    , Option ['p'] ["port"]     (ReqArg Port "PORT") "Run server on specified port (3000 by default)"
    , Option ['r'] ["readquotes"] (ReqArg DataFile "DATAFILE") "Read quotes from specified CSV file and write them to the DB"
    , Option ['D'] ["deletequotes"] (NoArg DelQuotes) "Delete ALL quotes from the DB"
    ]

appOpts :: [String] -> IO ([Flag], [String])
appOpts argv = 
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: web [OPTION...]"
                   
main = do
  runDb $ P.runMigration migrateTables

  createDefaultUser
  createDefaultPortfolio
  initializeDataTables
  
  let stock1 = "YHOO"
  let stock2 = "IBM"
  let datefrom = "2016-04-01"
  let dateto = "2016-05-06"
  let f = contrDate2Day $ at datefrom
  let t = contrDate2Day $ at dateto
 
  args <- getArgs
  params <- appOpts args
  port <- System.Environment.lookupEnv "PORT"
  case params of
    (opts@(o : _), _) -> mapM_ performAction opts
    ([],_) -> runServer (case port of
                            Just p -> (read p :: Int)
                            Nothing -> defaultPort)

performAction InitData = initData
performAction (Port portNum) = runServer $ read portNum
performAction (DataFile fileName) = insertFromCsv fileName
performAction DelQuotes = delQuotes

runServer port = scotty port $
  do
    api (url VO.vanillaOption) (jsonContract :: ActionM VO.VanillaOption) VO.makeContract
    api (url RO.rainbowOption) (jsonContract :: ActionM RO.RainbowOption) RO.makeContract
    api (url BO.basket2Option) (jsonContract :: ActionM BO.Basket2Option) BO.makeContract
    api (url DOB.doubleOptionBond) (jsonContract :: ActionM DOB.DoubleOptionBond) DOB.makeContract
    defaultService allContracts DBP.dbDataProvider

initData :: IO ()
initData = do
  currTime <- getCurrentTime
  let currDate = utctDay currTime
  putStrLn "Fetching initial data..."
  mapM_ (\x -> F.update_db_quotes x (formatDate (addDays (-90) currDate)) (formatDate currDate) "Yahoo") initialSymbols
  putStrLn "Done."
  return ()

delQuotes = runDb $ P.deleteWhere ([] :: [P.Filter DbQuotes])
         
insertFromCsv csvFile = do
  csvQuotes <- BL.readFile csvFile
  let quotes = case decode NoHeader csvQuotes of
                Left err -> error err
                Right v -> V.toList v
  -- insertMany_ is faster, but doest't work for large number of rows, so we just use insert.
  runDb $ mapM_ insert (map DBP.rawToDbQuotes quotes)
 
initializeDataTables = do
  quotes <- (runDb $ P.selectList [] []) :: IO [P.Entity DbQuotes]
  modelData <- (runDb $ P.selectList [] []) :: IO [P.Entity DbModelData]
  corr <- (runDb $ P.selectList [] []) :: IO [P.Entity DbCorr]
  when ((null quotes) && (null modelData) && (null corr)) $ DBP.insertFromCsv
