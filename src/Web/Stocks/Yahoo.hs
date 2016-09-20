module Stocks.Yahoo where

import Network.HTTP
import Data.List.Split

add_month "01" = "00"
add_month "02" = "01"
add_month "03" = "02"
add_month "04" = "03"
add_month "05" = "04"
add_month "06" = "05"
add_month "07" = "06"
add_month "08" = "07"
add_month "09" = "08"
add_month "10" = "09"
add_month "11" = "10"
add_month "12" = "11"

--Format of date: yyyy-mm-dd
gen_link id start_date end_date =
    "http://real-chart.finance.yahoo.com/table.csv?"        ++
    "s=" ++ id                                              ++
    "&a=" ++ add_month([start_date !! 5, start_date !! 6])  ++
    "&b=" ++ [start_date !! 8, start_date !! 9]             ++
    "&c=" ++ [start_date !! 0, start_date !! 1,
              start_date !! 2, start_date !! 3]             ++
    "&d=" ++ add_month([end_date !! 5, end_date !! 6])      ++
    "&e=" ++ [end_date !! 8, end_date !! 9]                 ++
    "&f=" ++ [end_date !! 0, end_date !! 1,
              end_date !! 2, end_date !! 3]                 ++
    "&g=d"                                                  ++ --time interval

    "&ignore=.csv"

-- Retrieves the data through HTTP and 
pull_data id start_date end_date = do
--    response <- simpleHTTP $ getRequest (gen_link id start_date end_date)
--MP
  let s = gen_link id start_date end_date
  --putStrLn s -- Print the request url
  response <- simpleHTTP $ getRequest (s)
  getResponseBody response

-- Splits on , and then keep date and 4th column = close value
-- Date, Open, Low, High, Close
--  0      1     2   3      4
get_data_csv xs = do
    let ys = splitOn "," xs
    (head(ys), ys !! 4)

get_data_csvAll xs = do
    let ys = splitOn "," xs
    (head(ys), ys !! 1,ys !! 2,ys !! 3,ys !! 4)

get_all_prices id start_date end_date = do
    s <- pull_data id start_date end_date
    let xs = splitOn "\n" s
    let ys = filter (/="") xs
    let result = map get_data_csvAll (tail(ys))
    return( result )
    
-- The function pulls the data from the service provider
-- and splits the data on new line
get_close id start_date end_date = do
    s <- pull_data id start_date end_date
    let xs = splitOn "\n" s
    let ys = filter (/="") xs
    return( map get_data_csv (tail(ys)))

