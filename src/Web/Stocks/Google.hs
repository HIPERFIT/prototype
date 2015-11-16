module Google where

import Network.HTTP
import Data.List.Split

month_name "01" = "Jan"
month_name "02" = "Feb"
month_name "03" = "Mar"
month_name "04" = "Apr"
month_name "05" = "May"
month_name "06" = "Jun"
month_name "07" = "Jul"
month_name "08" = "Aug"
month_name "09" = "Sep"
month_name "10" = "Oct"
month_name "11" = "Nov"
month_name "12" = "Dec"
month_name month = "error"

month_name_re "Jan" = "01"
month_name_re "Feb" = "02"
month_name_re "Mar" = "03"
month_name_re "Apr" = "04"
month_name_re "May" = "05"
month_name_re "Jun" = "06"
month_name_re "Jul" = "07"
month_name_re "Aug" = "08"
month_name_re "Sep" = "09"
month_name_re "Oct" = "10"
month_name_re "Nov" = "11"
month_name_re "Dec" = "12"
month_name_re month = "error"

format_date date =
    month_name([date !! 5 , date !! 6]) ++ "+" ++
    date !! 8 : date !! 9 : "+" ++
    date !! 0 : date !! 1 : date !! 2 : date !! 3 : ""

leading_zero n = if length n == 1 then "0" ++ n else n

--Format of date: yyyy-mm-dd
gen_link id start_date end_date =
    "http://www.google.com/finance/historical?" ++
    "q=" ++ id                                  ++
    "&startdate=" ++ format_date start_date     ++
    "&enddate=" ++ format_date end_date         ++
    "&output=csv"

pull_data id start_date end_date = do
    response <- simpleHTTP $ getRequest (gen_link id start_date end_date)
    getResponseBody response

get_data_csv xs = do
    let ys = splitOn "," xs
    let date_old = splitOn "-" (head ys)
    let date_new = "20" ++ leading_zero (date_old !! 2) ++ "-"   ++
                   month_name_re (date_old !! 1) ++ "-"          ++
                   leading_zero (date_old !! 0)
    (date_new, ys !! 4)

get_close id start_date end_date = do
    s <- pull_data id start_date end_date
    let xs = splitOn "\n" s
    let ys = filter (/="") xs
    return( map get_data_csv (tail(ys)))

