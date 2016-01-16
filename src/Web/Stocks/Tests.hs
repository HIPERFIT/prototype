module Stocks.Tests where

import Stocks.Google as Google
import Stocks.Yahoo as Yahoo
import Stocks.FetchStocks


main = do
    putStrLn $ Google.gen_link "GOOGL" "2015-12-01" "2015-12-01"
    putStrLn "-------------------------"
    putStrLn $ Yahoo.gen_link "GOOGL" "2015-12-01" "2015-12-01"
    putStrLn "-------------------------"
    Google.pull_data "GOOGL" "2015-12-01" "2015-12-01" >>= putStrLn
    putStrLn "-------------------------"
    Yahoo.pull_data "GOOGL" "2015-12-01" "2015-12-01" >>= putStrLn


