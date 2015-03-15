{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified VanillaOption as VO
import qualified RainbowOption as RO
import DataProviders.Database
import DataProviders.Data
import View
import Service
import PersistentData
import DB
import Serialization

import Data.Time
import Web.Scotty hiding (body, params)
import Data.Aeson (FromJSON)
import qualified Database.Persist.Sql as P
import System.Environment (getArgs)
import Control.Monad (when)

instance FromJSON VO.VanillaOption
instance FromJSON RO.RainbowOption

allContracts = [VO.vanillaOption, RO.rainbowOption]

main = do
  runDb $ P.runMigration migrateTables
  createDefaultUser
  createDefaultPortfolio
  initializeDataTables
  port <- getPortOrDefault
  scotty port $ do
    api "callOption"    (jsonContract :: ActionM VO.VanillaOption) VO.makeContract
    api "rainbowOption" (jsonContract :: ActionM RO.RainbowOption) RO.makeContract
    defaultService allContracts dbDataProvider

defaultPort = 3000
getPortOrDefault = do
  args <- getArgs
  return $ case args of
             []     -> defaultPort
             [port] -> read port
             _      -> error "Wrong number of arguments"

initializeDataTables = do
  quotes <- (runDb $ P.selectList [] []) :: IO [P.Entity DbQuotes]
  modelData <- (runDb $ P.selectList [] []) :: IO [P.Entity DbModelData]
  corr <- (runDb $ P.selectList [] []) :: IO [P.Entity DbCorr]
  when ((null quotes) && (null modelData) && (null corr)) $ insertFromCsv
