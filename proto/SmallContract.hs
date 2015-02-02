module SmallContract where

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
        theobs = obs ("Carlsberg",0)
        maturity = 357
    in scale (r 0.9997817434496459)
             (transl maturity
                    (iff (strike !<! theobs)
                         (scale (theobs - strike) (transfOne EUR "you" "me"))
                         zero))

mEuropean = (at "2014-01-01", european)

exampleUnderlyings = ["Carlsberg"]

-- Example data for pricing engine (similar to Medium contract in finpar)
exampleMarketData = (
  exampleCorrs,
  [Quotes "Carlsberg" [(at "2014-01-01", 3758.05)]]
  )


-- we are not using date information for now
-- that's why we can use arbitrary dates in model data
exampleModelData = [
  BS "Carlsberg"
  [(at "2014-01-01", 0.19, -0.0276481070940405)]
  ]

exampleCorrs = []

exampleDisc = ConstDisc 0.0200823

main = do
  prices <- runPricing (DataConf {monteCarloIter=1048576})  
                       [(exampleDisc, exampleModelData, exampleMarketData)] mEuropean
  putStrLn $ "Calculated prices: " ++ show prices

