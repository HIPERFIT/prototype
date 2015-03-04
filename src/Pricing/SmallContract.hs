-- Example contract and input similar to SmallContract in finpar
module SmallContract  where

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

mEuropean = (at "2011-12-09", european)

inputData :: [(DiscModel, ModelData, MarketData)]
inputData = [(ConstDisc 0.0200823,
             [BS "DJ_Eurostoxx_50" [(read "2012-11-30", 0.19, -0.0276481070940405)]],
             ([], [Quotes "DJ_Eurostoxx_50" [(read "2011-12-09", 3758.05)]]))
           ]

refPrice = 167.055714

main = do
  [price] <- runPricing (DataConf {monteCarloIter=1048576}) inputData mEuropean
  putStrLn $ "Calculated price: " ++ show price
  putStrLn $ "Actual price    : " ++ show refPrice

