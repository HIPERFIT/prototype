{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}
module Stocks.FetchStocks where

import Stocks.Yahoo as Yahoo
import Stocks.Google as Google
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


fetch id start_date end_date "Yahoo" = Yahoo.get_close id start_date end_date
fetch id start_date end_date "Google" = Google.get_close id start_date end_date
fetch id start_date end_date source = error "Not a valid source"

updateDatabase id start_date end_date source = do
    conn <- open "proto.sqlite3"
    execute conn "DELETE from stocks where stock_id=?" (Only(id :: String))
    fetch_data <- fetch id start_date end_date source
    mapM_ (insertToDb conn id) fetch_data
    close conn
    return(fetch_data)

insertToDb conn id (date, value) = do
    execute conn "INSERT INTO stocks (stock_id,date,value,created) VALUES(?,?,?,datetime('now'))"  ([id :: String, date :: String, value :: String])
    return (date, value)

fromDb id start_date end_date = do
    conn <- open "proto.sqlite3"
    rows <- query conn "SELECT date,value from stocks where stock_id=? and date>=? and date<=?" ([id :: String, start_date :: String, end_date :: String]) :: IO [(String,String)]
    return(rows)

stocks_outdated id start_date end_date = do
    conn <- open "proto.sqlite3"
    execute_ conn "CREATE TABLE IF NOT EXISTS stocks (id INTEGER PRIMARY KEY, stock_id TEXT, date TEXT, value TEXT, created DATETIME)"
    rows_startdate <- query conn "select value from stocks where date<=? and stock_id=? limit 1" ([start_date :: String, id :: String]) :: IO [Only(String)]
    rows_enddate <- query conn "select value from stocks where date>=? and stock_id=? limit 1" ([start_date :: String, id :: String]) :: IO [Only(String)]
    rows <- query conn "select value from stocks where strftime('%s','now') - strftime('%s',created) < 7200 and stock_id=? limit 1" (Only (id :: String)) :: IO [Only(String)]
    return(
        ((length rows)==0 && (length rows_enddate)==0) ||
        (length rows_startdate)==0)

getStocks :: String -> String -> String -> String -> IO [(String, String)]
getStocks id start_date end_date source = do
    b <- stocks_outdated id start_date end_date
    if b
    then updateDatabase id start_date end_date source
    else fromDb id start_date end_date

transToDbQuotes conn id (date,value) = do
    execute conn "INSERT INTO db_quotes (underlying,date,value,user_id) VALUES(?,?,?,1)"  ([id :: String, date :: String, value :: String])


update_db_quotes :: String -> String -> String -> String -> IO [(String, String)]
update_db_quotes id start_date end_date source = do
    conn <- open "proto.sqlite3"
    execute conn "DELETE from db_quotes where underlying=?" (Only(id :: String))
    stocks <- getStocks id start_date end_date source
    mapM_ (transToDbQuotes conn id) stocks
    return(stocks)
