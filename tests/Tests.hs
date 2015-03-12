module Tests where

import Contract
import Contract.Expr
import CodeGen.OpenclGen
import CodeGen.DataGen
import CodeGen.Utils
import Pricing
import Contract.Date
import qualified Config as Conf
import qualified SmallContract as SmallC
import qualified MediumContract as MediumC

import Test.HUnit
import System.IO (stderr, hPutStr)

{-
Ex 1a: Equity Call Option (no settlement period, no dividend)
Flirst code:
 flow(2009-04-01,"EUR",max(float(0.000000),sub(model("BASF",2009-04-01),float(45.000000))),"0")
S0 = 48
Risk free constant rate: 5%
Continuous discounting
Volatility (proportionality constant): 20%
Valuation date: 2008-09-01
Convention: Act/365
Expected Value: 5.437419207 by Black-Scholes closed form, 5.437414 by SCD Black-Scholes with 10^7 paths
FX's = []
-}

ex1a =
    let strike = r 45
        theobs = obs ("BASF", 0)
        maturity = 212
    in transl maturity (scale (maxx (theobs - strike) 0) (transfOne EUR "you" "me"))

mEx1a :: MContract
mEx1a = (read "2008-09-01", ex1a)

{- NOTE: almost all the parameters can be mapped straightforwardly on 
   corresponding input data for pricing engine,  but we need special treatment 
   of drift parameter in Black-Scholes model.
   For finpar pricing engine we should provide drift calculated as following:
   
       d = (r - (sigma^2)/2) * t

   where r - risk-free rate, sigma - volatility, t - time period (year fraction).
-}
ex1aData :: [(DiscModel, ModelData, MarketData)]
ex1aData = [(ConstDisc 0.05,
             [BS "BASF" [(read "2009-04-01", 0.2, 0.0174246575342466)]], -- drift calculated by formula above
             ([], [Quotes "BASF" [(read "2008-09-01", 48)]]))
           ]

ex1aPrice = 5.437419207

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
  Valuation date: 2008-09-01Kohlhagen
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
         decisionDays = 59 -- days before transfer
         theobs = obs (equity, -decisionDays)
     in
       transl maturity (iff (strike !<! theobs) 
                        (scale (theobs - strike) $ transfOne EUR "me" "you")
                        zero)

mEx2a :: MContract
mEx2a = (read "2008-09-01", ex2a)

ex2aData :: [(DiscModel, ModelData, MarketData)]
ex2aData = [(ConstDisc 0.05,
             [BS "BASF" [(read "2009-02-01", 0.2, 0.0125753424657534),
                         (read "2009-04-01", 0.2, 0.0174246575342466)]],
             ([], [Quotes "BASF" [(read "2008-09-01", 48)]]))
           ]

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

-- NOTE: probably, this contract cannot be priced correctly in present pricing model,
--       thus we will exclude it from test suite.
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

mEx4a :: MContract
mEx4a = (read "2008-09-01", ex4a)

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

mEx6a :: MContract
mEx6a = (read "2008-09-01", ex6a)

ex6aData :: [(DiscModel, ModelData, MarketData)]
ex6aData = [(ConstDisc 0.05,
             [BS "BASF" [(read "2009-04-01", 0.2, 0.0174246575342465)],
              BS "Siemens" [(read "2009-04-01", 0.15, 0.0225068493150685)]],
             ([], [Quotes "BASF" [(read "2008-09-01", 48)],
                   Quotes "Siemens" [(read "2008-09-01", 46)]]))
           ]

ex6aPrice = 7.1530898461

{-
Ex 6c: Rainbow Option - Max-of (no settlement period, no dividend, no strike)
Flirst code:
 flow(2009-04-01,"EUR",max(float(0.000000),max(model("Siemens",2009-04-01),model("BASF",2009-04-01))),"0")
S_A = 48
S_B = 46
Risk free constant rate 5%
Continuous discounting
Vol_A = 20%
Vol_B = 15%
No correlation
Valuation date: 2008-09-01
Convention: Act/365
Expected Value: 50.6554209353 by closed form (Black-Scholes model), 50.655414 by SCD Black-Scholes MC with 10^7 paths
FX's = []
-}

ex6c = 
    let
        basf = obs ("BASF", 0)
        siemens = obs ("Siemens", 0)
        maturity = 212 -- dateDiff (read "2008-09-01") (read "2009-04-01")
        val = maxx 0 (maxx siemens basf)
    in
      transl maturity $ scale val $ transfOne EUR "me" "you"

mEx6c :: MContract
mEx6c = (read "2008-09-01", ex6c)

ex6cPrice = 50.6554209353

{-
Ex 7a: FX Option (no settlement period)
Flirst code:
 flow(2009-04-01,"EUR",
      max(float(0.000000),
          sub(model("FX_EUR_CAD",2009-04-01),
              float(0.900000))),"0")
FX0 = 0.8
Risk free constant rate for EUR: 5%
Risk free constant rate for CAD: 4%
Continuous discounting
FX Volatility (proportionality constant): 20%
Valuation date: 2008-09-01
Convention: Act/365
Expected Value: 0.016941999 by closed form Garman-Kohlhagen
FX's = [EUR/CAD]
-}

ex7a =
    let theobs = obs ("FX_EUR_CAD", 0)
        maturity = 212
        strike = r 0.9
    in transl maturity (scale (maxx (theobs - strike) 0) (transfOne EUR "you" "me"))

mEx7a :: MContract
mEx7a = (read "2008-09-01", ex7a)

{- NOTE: FX option expressed in contract language is similar to ordinary call option.
   Difference can be observed in input data. For FX option we need to take into considiration
   two risk-free rates: domestic (rd) and foreign (rf) (5% and 4% for ex7a correspondingly). In this case
   we use the following formula for drift parameter for our model:

         d = (rd - rf - (sigma^2)/2) * t
   
   For discounting of calculated price we use domestic rate rd.
   See: Andreas Weber and Uwe Wystup. Pricing Formulae for Foreign Exchange Options.
        http://www.researchgate.net/publication/228007079_Pricing_Formulae_for_Foreign_Exchange_Options
-}
ex7aData :: [(DiscModel, ModelData, MarketData)]
ex7aData = [(ConstDisc (0.05),
             [BS "FX_EUR_CAD" [(read "2009-04-01", 0.2, -0.005808219178082192)]],
             ([], [Quotes "FX_EUR_CAD" [(read "2008-09-01", 0.8)]]))
           ]

ex7aPrice = 0.016941999


testData =
    [("Ex1a", ex1aData, mEx1a, ex1aPrice),
     ("Ex2a", ex2aData, mEx2a, ex2aPrice),
     --("Ex4a", ex2aData, mEx4a, ex4aPrice),
     ("Ex6a", ex6aData, mEx6a, ex6aPrice),
     ("Ex6c", ex6aData, mEx6c, ex6cPrice),
     ("Ex7a", ex7aData, mEx7a, ex7aPrice),
     ("finpar: SmallContract", SmallC.inputData, SmallC.mEuropean, SmallC.refPrice),
     ("finpar: MediumContract", MediumC.inputData, MediumC.worstOff, MediumC.refPrice)
    ]

dataConf = DataConf {monteCarloIter = 4000000}

relError v refV = (abs (refV - v)) / refV

eqUptoEps a b eps = (relError a b) <= eps

eps = 0.0005 -- acceptable level of error

hUnitTestPricing (inputData, mContr, refPrice) isVerbose = 
    TestCase (
              do 
                [price] <- runPricing dataConf inputData mContr
                assertBool ("Error is more than " ++ (ppDouble 3 (eps * 100)) ++ "%"  ++ 
                            "\nCurrent value: " ++ (show price) ++ 
                            "\nShould be:     " ++ (show refPrice))
                           (eqUptoEps price refPrice eps)
                if isVerbose then (verboseOut price) else putStr ""
             )
    where
      verboseOut price= do
        putStrLn $ "Calculated price: " ++ show price
        putStrLn $ "Actual price:     " ++ show refPrice
        putStrLn $ "Error:            " ++ (ppDouble 5 ((relError price refPrice) * 100) ++ "%")
        putStrLn ""

printCode  =  putStr . ppCLSeq . genPayoffFunc

mkTest (name, inputData, mContr, refPrice) = TestLabel name $ 
                                             hUnitTestPricing (inputData, mContr, refPrice) True

tests = TestList $ map mkTest testData

-- modified version of original runTestText from HUnit: prints test labels for every test.
runTestText' :: PutText st -> Test -> IO (Counts, st)
runTestText' (PutText put us0) t = do
  (counts', us1) <- performTest reportStart reportError reportFailure us0 t
  us2 <- put (showCounts counts') True us1
  return (counts', us2)
 where
  reportStart ss us = put ("Running " ++ showPath (path ss)) True us
  reportError   = reportProblem "Error:"   "Error in:   "
  reportFailure = reportProblem "Failure:" "Failure in: "
  reportProblem p0 p1 msg ss us = put line True us
   where line  = "### " ++ kind ++ path' ++ '\n' : msg
         kind  = if null path' then p0 else p1
         path' = showPath (path ss)

runTests = runTestText' (putTextToHandle stderr False) tests

main = runTests
