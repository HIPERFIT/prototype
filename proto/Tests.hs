module Tests where

import Contract
import Contract.Expr
import CodeGen.OpenclGen
import CodeGen.DataGen
import CodeGen.Utils
import Pricing
import Contract.Date
import System.Process
import System.Directory
import qualified Config as Conf

{-
 Ex 2a: Equity Call Option (with settlement period, no dividend)
  Flirst code:
  choosebest(flow(2009-04-01,"EUR",sub(model("BASF",2009-02-01),float(45.)),"0"),
             many(),
             2009-02-01)
  S0 = 48
  Risk free constant rate: 5%
  Continuous discounting
  Volatility (proportionality constant): 20%
  Valuation date: 2008-09-01
  Convention: Act/365
  Dividend yield: 4%
  Expected Value: 4.802921344 by Black-Scholes closed form
  FX's = []
-}

ex2a = 
     let
         equity = "BASF"
         maturity = 212 -- dateDiff (read "2008-09-01") (read "2009-04-01")
         strike = r 45
         decisionDays = 2 -- days before transfer
         theobs = obs (equity, -decisionDays)
     in
       transl maturity (iff (strike !<! theobs) 
                        (scale (theobs - strike) $ transfOne EUR "me" "you")
                        zero)

mEx2a :: MContract
mEx2a = (read "2008-09-01", ex2a)

ex2aPrice = 4.802921344

 
{-
 Ex 4a: Simple Chooser option (same strike, fixing date, mat date for put and call parts, no dividend)
  Flirst code:
 bind("4",float(45.),
 bind("5",model("BASF",2009-04-01),
 choosebest(flow(2009-04-01,"EUR",sub(use("5"),use("4")),"0"),
            flow(2009-04-01,"EUR",sub(use("4"),use("5")),"0"),
            2009-02-01)))
  S0 = 48
  Risk free constant rate: 5%
  Continuous discounting
  Volatility (proportionality constant): 20%
  Valuation date: 2008-09-01
  Convention: Act/365
  Expected Value: 6.251815401 by closed form (Black-Scholes model), 6.251861 by SCD Black-Scholes MC with 10^7 paths
  FX's = []
 -}

ex4a = 
    let
        equity = "BASF"
        maturity = 212 -- dateDiff (read "2008-09-01") (read "2009-04-01")
        strike = r 45
        decisionDays = 59 -- days before transfer
        theobs1 = obs (equity, -decisionDays)
        theobs2 = obs (equity, 0)
        amount1 = theobs1 - strike
        amount2 = strike - theobs1
    in transl maturity (iff (amount1 !<! amount2)
                        (scale (maxx (strike - theobs2) 0) $ transfOne EUR "me" "you")
                        (scale (maxx (theobs2 - strike) 0) $ transfOne EUR "me" "you"))

ex4a' = 
    let
        equity = "BASF"
        maturity = 212 -- dateDiff (read "2008-09-01") (read "2009-04-01")
        strike = r 45
        decisionDays = 59 -- days before transfer
        theobs2 = obs (equity, 0)
        amount1 = theobs2 - strike
        amount2 = strike - theobs2
    in transl (maturity - decisionDays) (iff (amount1 !<! amount2)
                   (transl decisionDays $ scale (maxx amount2 0) $ transfOne EUR "me" "you")
                   (transl decisionDays $ scale (maxx amount1 0) $ transfOne EUR "me" "you"))


mEx4a :: MContract
mEx4a = (read "2008-09-01", ex4a)

ex4aData :: [(DiscModel, ModelData, MarketData)]
ex4aData = [(ConstDisc 0.05,
             [BS "BASF" [(read "2009-02-01", 0.2, 0),
                         (read "2009-04-01", 0.2, 0)]],
             ([], [Quotes "BASF" [(read "2008-09-01", 48)]]))
           ]

ex4aPrice = 6.251815401

{-
  Ex 6a: Rainbow Option - Max-of (no settlement period, no dividend)
  Flirst code:
  flow(2009-04-01,"EUR", 
       max(float(0.),sub(max(model("Siemens",2009-04-01),
                         model("BASF",2009-04-01)),float(45.))),"0")
  S_A = 48
  S_B = 46
  Risk free constant rate 5%
  Continuous discounting
  Vol_A = 20%
  Vol_B = 15%
  No correlation
  Valuation date: 2008-09-01
  Convention: Act/365
  Expected Value: 7.1530898461 by closed form (Black-Scholes model)
-}

ex6a = 
    let
        basf = obs ("BASF", 0)
        siemens = obs ("Siemens", 0)
        maturity = 212 -- dateDiff (read "2008-09-01") (read "2009-04-01")
        strike = r 45
        val = maxx 0 ((maxx siemens basf) - strike)
    in
      transl maturity $ scale val $ transfOne EUR "me" "you"

mEx6a = (read "2008-09-01", ex6a)

ex6aData :: [(DiscModel, ModelData, MarketData)]
ex6aData = [(ConstDisc 0.05,
             [BS "BASF" [(read "2009-04-01", 0.2, 0)],
              BS "Siemens" [(read "2009-04-01", 0.15, 0)]],
             ([], [Quotes "BASF" [(read "2008-09-01", 48)],
                   Quotes "Siemens" [(read "2008-09-01", 46)]]))
           ]

ex6aPrice = 7.1530898461

tests = 
    [("Ex2a", ex4aData, mEx2a, ex2aPrice), --using same data set as for ex4a
     ("Ex4a", ex4aData, mEx4a, ex4aPrice),
     ("Ex6a", ex6aData, mEx6a, ex6aPrice)
    ]

dataConf = DataConf {monteCarloIter = 1000000}

testPricing (name, inputData, mContr, refPrice) = 
    do
      [price] <- runPricing dataConf inputData mContr
      putStrLn $ "---- " ++ name ++ " ----"
      putStrLn $ "Calculated price: " ++ show price
      putStrLn $ "Actual price:     " ++ show refPrice
      putStrLn $ "Diff:             " ++ show (refPrice - price)

printCode  =  putStr . ppCLSeq . genPayoffFunc
      
main = mapM_ testPricing tests 
