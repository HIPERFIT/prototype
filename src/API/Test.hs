module Main where

import Google as Google
import Yahoo as Yahoo
import FetchStocks

main = do
    s <- getStocks "AAPL" "2015-09-28" "2015-10-05" "Yahoo"
    putStrLn(fst(s !! 0))
    putStrLn(snd(s !! 0))

