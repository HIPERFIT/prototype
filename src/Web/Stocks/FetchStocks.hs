{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}
module Stocks.FetchStocks where

import Stocks.Yahoo as Yahoo
import Stocks.Google as Google
import DB as DB
import Data.Text (Text, unpack, pack)
import Data.List
import Database.Persist.Sqlite
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Utils

fetch :: String -> String -> String -> String -> IO [(String, String)]
fetch id start_date end_date "Yahoo" = Yahoo.get_close id start_date end_date
fetch id start_date end_date "Google" = Google.get_close id start_date end_date
fetch id start_date end_date _ = error "Not a valid source"

updateDatabase :: String -> String -> String -> String -> IO [(String, String)]
updateDatabase id start_date end_date source = do    
    DB.runDb $ rawExecute "DELETE from stocks where stock_id=?" [toPersistValue id]
    fetch_data <- fetch id start_date end_date source
    DB.runDb $ sequence (map (insertToDb id) fetch_data)
    return fetch_data

insertToDb id (date, value) = 
 rawExecute "INSERT INTO stocks (stock_id,date,value,created) VALUES(?,?,?,datetime('now'));\nINSERT INTO stocks (stock_id,date,value,created) VALUES(?,?,?,datetime('now'))"
                (map toPersistValue [id, date, value])

fromDb id start_date end_date = do    
    rows <- DB.runDb $ rawSql "SELECT date,value from stocks where stock_id=? and date>=? and date<=?"
            (map toPersistValue [id, start_date, end_date])
    return (map (\(x,y) -> (unpack $ unSingle x, unpack $ unSingle y)) rows)

fromDb' id start_date = do    
    rows <- DB.runDb $ rawSql "SELECT date,value from stocks where stock_id=? and date>=?"
            (map toPersistValue [id, start_date])
    return (map (\(x,y) -> (unpack $ unSingle x, unpack $ unSingle y)) rows)

fromDb'' id = do    
    rows <- DB.runDb $ rawSql "select value, (strftime('%s','now') - strftime('%s',created)) from stocks where strftime('%s','now') - strftime('%s',created) < 7200  and stock_id=? limit 1"
            [toPersistValue id]
    return (map (\(x,y) -> (unpack $ unSingle x, unpack $ unSingle y)) rows)

stocks_outdated id start_date end_date = do
    DB.runDb $ rawExecute
          "CREATE TABLE IF NOT EXISTS stocks (id INTEGER PRIMARY KEY, stock_id TEXT, date TEXT, value TEXT, created DATETIME)" []
    rows_startdate <- DB.runDb $ rawSql "select value from stocks where date<=? and stock_id=? limit 1"
                      (map toPersistValue [start_date, id])
    -- TODO: probably, here should be end_date!
    rows_enddate <- DB.runDb $ rawSql "select value from stocks where date>=? and stock_id=? limit 1"
                    (map toPersistValue [start_date, id])
    rows <- DB.runDb $ rawSql
            "select value from stocks where strftime('%s','now') - strftime('%s',created) < 7200 and stock_id=? limit 1" []
    -- TODO: do better check if whether query result is empty!
    return (
            ((length (map (unpack . unSingle) rows))==0 &&
              (length (map (unpack . unSingle) rows_enddate))==0)
            || (length (map (unpack . unSingle) rows_startdate))==0)

getStocks :: String -> String -> String -> String -> IO [(String, String)]
getStocks id start_date end_date source = do
    b <- stocks_outdated id start_date end_date
    if b
    then updateDatabase id start_date end_date source
    else fromDb id start_date end_date

transToDbQuotes id (date,value) =
     rawExecute "INSERT INTO db_quotes (underlying,date,value,user_id) VALUES(?,?,?,1)" (map toPersistValue [id, date, value])

update_db_quotes :: String -> String -> String -> String -> IO [(String, String)]
update_db_quotes id start_date end_date source = do
    DB.runDb $ rawExecute "DELETE from db_quotes where underlying=?" [toPersistValue id]
    stocks <- getStocks id start_date end_date source
    DB.runDb $ sequence_ (map (transToDbQuotes id) stocks)
    return stocks
