module Yahoo where

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

pull_data id start_date end_date = do
    response <- simpleHTTP $ getRequest (gen_link id start_date end_date)
    getResponseBody response

get_data_csv xs = do
    let ys = splitOn "," xs
    (head(ys), ys !! 4)

get_close id start_date end_date = do
    s <- pull_data id start_date end_date
    return (map get_data_csv (tail(splitOn "\n" s)))
