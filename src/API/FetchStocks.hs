{-# LANGUAGE OverloadedStrings #-}
module FetchStocks where

import Yahoo as Yahoo
import Google as Google
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


fetch id start_date end_date "Yahoo" = Yahoo.get_close id start_date end_date
fetch id start_date end_date "Google" = Google.get_close id start_date end_date
fetch id start_date end_date source = error "Not a valid source"

updateDatabase id start_date end_date source = do
    conn <- open "proto.sqlite3"
    execute_ conn "CREATE TABLE IF NOT EXISTS stocks (id INTEGER PRIMARY KEY, stock_id TEXT, date TEXT, value TEXT )" --TODO: move this to an init file
    execute conn "DELETE from stocks where stock_id=?" (Only(id :: String))
    fetch_data <- fetch id start_date end_date source
    mapM_ (insertToDb conn id) fetch_data
    close conn
    return(fetch_data)

insertToDb conn id (date, value) = do
    execute conn "INSERT INTO stocks (stock_id,date,value) VALUES(?,?,?)"  ([id :: String, date :: String, value :: String])
    return (date, value)

