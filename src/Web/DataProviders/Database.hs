{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}
module DataProviders.Database where

import DB
import DataProviders.Data
import qualified DataProviders.Csv as Csv
import PersistentData

import Data.Time
import Data.Text (Text, unpack, pack)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql (rawSql, Single, unSingle, toSqlKey)
import GHC.Generics
import Control.Monad (forM, liftM)
import Control.Monad.Trans (lift)

dbDataProvider = DataProvider { provideQuotes     = getRawQuotes
                              , provideCorrs      = undefined
                              , provideModelData  = getRawModelData
                              , storedQuotes      = getStoredQuotes
                              , storedCorrs       = undefined
                              , storedModelData   = getStoredModelData
                              , storedUnderlyings = availUnderlyings }            

getRawQuotes :: [Day] -> String -> IO [RawQuotes]
getRawQuotes ds und = mapM ((liftM dbQuotesToRaw) . (getClosestQuote und)) ds

getClosestQuote und d = do
  res <- runDb $ selectList [DbQuotesUnderlying ==. (pack und), DbQuotesDate <=. d] [Desc DbQuotesDate, LimitTo 1]
  return $ case res of
    [] -> error ("No quote for " ++ show d)
    [Entity k (DbQuotes und _ v u)] -> DbQuotes und d v u

getRawModelData :: [Day] -> String -> IO [RawModelData]
getRawModelData ds und  = mapM ((liftM dbMdToRaw) . (getClosestMd und)) ds

getClosestMd und d = do
  res <- runDb $ selectList [DbModelDataUnderlying ==. (pack und), DbModelDataDate <=. d] [Desc DbModelDataDate, LimitTo 1]
  return $ case res of
    [] -> error ("No model data for " ++ show d)
    [Entity k (DbModelData und _ v u)] -> DbModelData und d v u

getStoredQuotes :: IO [RawQuotes]
getStoredQuotes = do 
  q <- (runDb $ selectList [] []) :: IO [Entity DbQuotes]
  return $ map (dbQuotesToRaw . entityVal) q

getStoredModelData :: IO [RawModelData]
getStoredModelData = do 
  md <- (runDb $ selectList [] []) :: IO [Entity DbModelData]
  return $ map (dbMdToRaw . entityVal) md

availUnderlyings :: IO [String]
availUnderlyings = do
  res <- runDb $ rawSql (pack "SELECT DISTINCT underlying FROM db_quotes" ) []
  return $ map (unpack . unSingle) res

-- inserting all csv data provided by DataProviders.Csv into database
insertFromCsv = do
  sq <-  storedQuotes Csv.csvDataProvider
  smd <- storedModelData Csv.csvDataProvider
  runDb $ insertMany_ (map rawToDbQuotes sq)
  runDb $ insertMany_ (map rawToDbMd smd)


-- conversion functions to and from raw data representation

dbQuotesToRaw q = (unpack $ dbQuotesUnderlying q, dbQuotesDate q, dbQuotesValue q)
rawToDbQuotes (und, d, v) = DbQuotes (pack und) d v $ toSqlKey (fromIntegral defaultUserId)

dbMdToRaw md = (unpack $ dbModelDataUnderlying md, dbModelDataDate md, dbModelDataValue md)
rawToDbMd (und, d, v) = DbModelData (pack und) d v $ toSqlKey (fromIntegral defaultUserId)
