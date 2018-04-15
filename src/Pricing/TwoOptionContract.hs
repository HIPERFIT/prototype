-- | Input data for the Futhark pricer (still requires modifications by hand)

module Pricing.TwoOptionContract where

import Contract
import CodeGen.OpenclGen
import CodeGen.DataGen
import CodeGen.Utils
import Pricing
import Contract.Date
import System.Process
import System.Directory
import qualified Config as Conf

-- this contract corresponds to the small contract in finpar
european =
    let strike = r 4000
        theobs = obs ("DJ_Eurostoxx_50",0)
        maturity = 365
    in transl maturity
       (iff (strike !<! theobs)
                      (scale (theobs - strike) (transfOne EUR "you" "me"))
                      zero)
european' =
    let strike = r 2000
        theobs = obs ("AAPL",0)
        maturity = 100
    in (transl maturity
         (iff (strike !<! theobs)
           (scale (theobs - strike) (transfOne EUR "you" "me"))
           zero))

mTwoOptC = (at "2011-01-01", both european european')

inputData :: [(DiscModel, ModelData, MarketData)]
inputData = [(ConstDisc 0.0200823,
             [BS "DJ_Eurostoxx_50" [(read "2012-01-01", 0.19, -0.0276481070940405),
                                    (read "2012-04-11", 0.19, -0.0276481070940405)]],
             ([], [Quotes "DJ_Eurostoxx_50" [(read "2011-01-01", 3758.05)]])),
             (ConstDisc 0.0180823,
             [BS "AAPL" [(read "2011-01-01", 0.10, -0.0176481070940405),
                         (read "2011-04-11", 0.10, -0.0176481070940405)]],
             ([], [Quotes "AAPL" [(read "2011-01-01", 2758.05)]]))
           ]

refPrice = 167.055714

main = do
  gd <- generateData (DataConf {monteCarloIter=1048576}) inputData mTwoOptC
  putStrLn $ gd

