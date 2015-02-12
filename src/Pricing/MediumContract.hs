-- Example contract and input similar to MediumContract in finpar
module MediumContract where

import Contract
import CodeGen.OpenclGen
import CodeGen.DataGen
import CodeGen.Utils
import Pricing

-- Taken from "contracts" repository. 
-- LexiFi contract translated to contract DSL by Jost Berthold.
-- worst-off contract on five fixed dates (chain of iff).
worstOff :: MContract
worstOff = (start, foldr mkDateCheck endCase (zip dDiffs premiums))
    where start  = at "2012-01-27"
          dates  = map (\s -> at (show s ++ "-01-27")) [2013..2017]
          dDiffs   = zipWith dateDiff (start:dates) dates
          premiums = [1150.0, 1300.0, 1450.0, 1600.0, 1750]
          -- on the five dates (offset): one below initial spot => pay premium
          mkDateCheck (offset, premium) cont
              = transl offset $ iff barrier (collectEUR premium) cont
          barrier = nott (foldl1 minn (zipWith mkDiff idxs spots) !<! 0)
          -- MEMO we should have <= , > and >= as smart constructors
          mkDiff idx spot = obs (idx, 0) - spot
           -- MEMO we should have RealE division.
          idxs   = [ "DJ_Eurostoxx_50", "Nikkei_225", "SP_500" ]
          spots  = [ 3758.05, 11840, 1200 ]
          -- if end (date 5) reached: pay 1000 if all above 0.75,
          -- otherwise pay the fraction of the worst (HOW? no division)
          endCase = iff (allAbove 0.75) (collectEUR 1000) 
                        (collectEUR (1000 * minRatio))
          minRatio = foldl1 minn 
                            (zipWith (\id sp -> obs(id,0) / sp) idxs spots)
          allAbove d = nott (foldl1 (!|!) 
                             (zipWith (fractionSmaller d) idxs spots))
           {- 0.75 < minimum [ obs(id,0) / sp | (id, sp) <- zip idxs spots ]
                               <==> 
              and [ 0.75 * sp !<! obs (id, 0) | (id, sp) <- zip idxs spots ]
                               <==> 
            not (or [obs(id, 0) !<! 0.75 * sp | (id, sp) <- zip idxs spots]) -}
          fractionSmaller d idx spot = obs(idx, 0) !<! d * spot
          collectEUR amount = scale amount (transfOne EUR "them" "us")


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

inputData = [(exampleDisc, exampleModelData, exampleMarketData)]
refPrice = 937.3915829436

-- usage examples
-- putStr $ ppCLSeq $ genPayoffFunc ex2 -- pretty-printing in console
-- writeOpenCL (ppCLSeq $ genPayoffFunc ex2) "SmallContract" -- to generate SmallContract.cl in current directory

-- calculate price for worstOff LexiFi contract (Medium contract in finpar)
main = do
  [price] <- runPricing (DataConf {monteCarloIter=1048576}) inputData worstOff
  putStrLn $ "Calculated price: " ++ show price
  putStrLn $ "Actual price:     " ++ show refPrice
