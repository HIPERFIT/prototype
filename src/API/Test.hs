module Main where

import Google as Google
import Yahoo as Yahoo

main = do
    s <- Yahoo.get_close "AAPL" "2015-09-28" "2015-10-05"
    putStrLn(fst(s !! 0))
    putStrLn(snd(s !! 0))
    s <- Google.get_close "AAPL" "2015-09-28" "2015-10-05"
    putStrLn(fst(s !! 0))
    putStrLn(snd(s !! 0))

