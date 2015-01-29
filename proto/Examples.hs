module CodeGen.Examples where

import Contract
import LexifiContracts
import CodeGen.OpenclGen
import CodeGen.DataGen
import CodeGen.Utils
import Pricing

-- Sample contracts

ex1 = scale 200 $ flow 1 (r 100) EUR "you" "me"

ex2 =
    let strike = 4000.0
        theobs = obs ("Carlsberg",0)
    in scale (r 0.9997817434496459)
             (transl 360
                    (iff (r strike !<! theobs)
                          (scale (theobs - r strike)
                                 (transfOne EUR "you" "me"))
                         zero))

-- like ex2 but with additional condition (just to test nested "if"s)
ex3 =
    let strike = 4000.0
        theobs = obs ("Carlsberg",0)
    in scale (r 0.9997817434496459)
             (transl 360
                    (iff (r strike !<! theobs)
                          ( iff (r strike !<! theobs * 2)
                            (scale (theobs*2 - r strike) (transfOne EUR "you" "me"))
                            (scale (theobs - r strike)
                                 (transfOne EUR "you" "me")))
                         zero))

ex4 =
    let strike = 4000.0
        theobs = obs ("Carlsberg",0)
        theobs1 = obs ("Carlsberg",1)
    in scale (r 0.9997817434496459)
             (transl 360
                    (iff (r strike !<! theobs1)
                          (transl 360 $
                           iff (r strike !<! theobs * 2)
                               (scale (theobs*2 - r strike) (transfOne EUR "you" "me"))
                               (scale (theobs1 - r strike)
                                      (transfOne EUR "you" "me")))
                           (transl 180 $ (iff (r strike !<! theobs - 10)
                                              (transl 185 $ scale theobs $ transfOne EUR "you" "me")
                                          zero))))
equity = "Carlsberg"
maturity = 2
ex5 = checkWithin (strike !<! theobs) maturity
                    (scale (theobs - strike) (transfOne EUR "you" "me")) zero
    where strike = r 50.0
          theobs = obs (equity,0)

-- Example data for pricing engine (similar to Medium contract in finpar)
exampleMarketData = (
  exampleCorrs,
  [Quotes "DJ_Eurostoxx_50" [(at "2012-01-27", 3758.0500000000001819)],
   Quotes "Nikkei_225" [(at "2012-01-27", 11840.0)],
   Quotes "SP_500" [(at "2012-01-27", 1200.0)]]
  )

exampleUnderlyings = ["DJ_Eurostoxx_50", "Nikkei_225", "SP_500"]

-- we are not using date information for now
-- that's why we can use arbitrary dates in model data
exampleModelData = [
  BS "DJ_Eurostoxx_50"
  [(at "2012-01-27", 0.19, -0.0283491736871803),
   (at "2012-01-27", 0.19, -0.0183841413744211),
   (at "2012-01-27", 0.19, -0.0172686581005089),
   (at "2012-01-27", 0.19, -0.0144179417871814),
   (at "2012-01-27", 0.19, -0.0121497422218761)],
  BS "Nikkei_225"
  [(at "2012-01-27", 0.19, 0.0178771081725381),
   (at "2012-01-27", 0.19, -0.0044530897672834),
   (at "2012-01-27", 0.19, 0.0125638544546015),
   (at "2012-01-27", 0.19, 0.0157411263968213),
   (at "2012-01-27", 0.19, 0.0182904634062437)],
  BS "SP_500"
  [(at "2012-01-27", 0.15, 0.0043096808044729),
   (at "2012-01-27", 0.15, 0.0024263805987983),
   (at "2012-01-27", 0.15, 0.0094452810918001),
   (at "2012-01-27", 0.15, 0.0125315353728014),
   (at "2012-01-27", 0.15, 0.0151125070556484)]
  ]

exampleCorrs = [Corr ("DJ_Eurostoxx_50", "Nikkei_225") 0.6, 
                Corr ("DJ_Eurostoxx_50", "SP_500") 0.8, 
                Corr ("Nikkei_225", "SP_500") 0.6]

exampleDisc = CustomDisc [ (366, 0.9797862861805930), (731, 0.9505748482484491), (1096, 0.9214621679912968)
                         , (1461, 0.8906693055891434), (1827, 0.8588567633110704)]

origResult = "[937.3915829436]"

-- usage examples
-- putStr $ ppCLSeq $ genPayoffFunc ex2 -- pretty-printing in console
-- writeOpenCL (ppCLSeq $ genPayoffFunc ex2) "SmallContract" -- to generate SmallContract.cl in current directory

-- calculate price for worstOff LexiFi contract (Medium contract in finpar)
main = do
  prices <- runPricing (DataConf {monteCarloIter=1048576}) [--(exampleDisc, exampleModelData, exampleMarketData), 
                        (exampleDisc, exampleModelData, exampleMarketData)] worstOff
  putStrLn $ "Calculated price: " ++ show prices
  putStrLn $ "Actual price:     " ++ origResult
