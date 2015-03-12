{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified CallOption as CO
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

instance FromJSON CO.CallOption
instance FromJSON RO.RainbowOption

allContracts = [CO.callOption, RO.rainbowOption]

main = do
  runDb $ P.runMigration migrateTables
  port <- getPortOrDefault
  scotty port $ do
    api "callOption"    (jsonContract :: ActionM CO.CallOption) CO.makeContract
    api "rainbowOption" (jsonContract :: ActionM RO.RainbowOption) RO.makeContract
    defaultService allContracts dbDataProvider

defaultPort = 3000
getPortOrDefault = do
  args <- getArgs
  return $ case args of
             []     -> defaultPort
             [port] -> read port
             _      -> error "Wrong number of arguments"
