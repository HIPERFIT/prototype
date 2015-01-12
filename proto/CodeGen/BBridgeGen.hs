{-# OPTIONS -XRecordWildCards #-}
-- taken from LexiFi project repository
module CodeGen.BBridgeGen where

import System.Environment

import Data.List(intersperse)

import qualified Data.Vector as V
import Data.Vector(Vector, (!))

-- used in brownian_bridge_gen
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST

import CodeGen.Syntax

-- this should correspond closely to PricingTypes.hs (field names must match)
data Pricing_Data a = Pricing_Data {
         -- Model Data: Observables, dates, relationship
         md_dim             :: Int, -- no. of observables
         md_nb_path_dates   :: Int, -- no. of dates to consider

         -- brownian bridge parameters (RealE is an expression)
         bb_l               :: Int,
         bb_sd              :: Vector a,
         bb_lw              :: Vector a,
         bb_rw              :: Vector a,
         bb_bi              :: Vector Int,
         bb_li              :: Vector Int,
         bb_ri              :: Vector Int
        }
       deriving (Show,Eq)

abstract_init :: Int -> Int -> Pricing_Data RealE
abstract_init dim n_dates = Pricing_Data {
                         md_dim = dim
                       , md_nb_path_dates = n_dates
                       --------------------------
                       , bb_l = n_dates
                       , bb_sd = abstract_vector "bb_sd" n_dates
                       , bb_lw = abstract_vector "bb_lw" n_dates
                       , bb_rw = abstract_vector "bb_rw" n_dates
                       , bb_bi = V.fromList bi
                       , bb_li = V.fromList li
                       , bb_ri = V.fromList ri
                       }
    where indices    = bbridge_auxI 0 n_dates
          (li,bi,ri) = unzip3 indices

abstract_vector :: String -> Int -> Vector RealE
abstract_vector name len = V.fromList
                       [ var (name ++ " V.! " ++ show k) | k <- [0..len-1] ]

----------------------------------------------------------------------------
-- BBridgeCreate:

abstract_dates :: (RealE, [RealE])
abstract_dates = (var "today",
                  [ var ("(dates!!" ++ show k ++ ")") | k <- [0..4]])
-- Dates in input are given as minutes since 1980, need to be adjusted to years
-- from the pricing day
bbridgeConf :: (Floating a, Num a) => Int -> (a,[a]) -> Pricing_Data a
bbridgeConf dim (today, dates)
    = Pricing_Data {  md_dim = dim
                    , md_nb_path_dates = len
                    , bb_l = len
                    , bb_sd = sd
                    , bb_lw = lw
                    , bb_rw = rw
                    , bb_bi = V.fromList bi
                    , bb_li = V.fromList li
                    , bb_ri = V.fromList ri }
    where len        = length dates
          indices    = bbridge_auxI 0 len
          (li,bi,ri) = unzip3 indices
          (lw,rw,sd) = bbridge_auxD (V.fromList dates_since) indices
          dates_since = map (in_years . (\x -> x - today)) dates
in_years :: (Floating a) => a -> a
in_years date = date / (365.0 * 1440.0)

-- build index triples li, bi, ri for bbridge
bbridge_auxI :: Int -> Int -> [(Int,Int,Int)]
bbridge_auxI lo hi = (0,hi,0):mkIndices lo hi

mkIndices lo hi = if lo < mid && mid < hi 
                  then (lo,mid,hi):lower ++ higher
                  else []
    where mid = lo + (hi-lo) `div` 2 
          lower  = mkIndices lo mid
          higher = mkIndices mid hi

-- double vectors lw, rw, sd
bbridge_auxD :: Floating a => Vector a -> [(Int,Int,Int)] -> 
                (Vector a,Vector a,Vector a)
bbridge_auxD dates indices = V.unzip3 (V.map mkAverage (V.fromList indices))
    where mkAverage (0, mid, 0) = (0, 0, sqrt (dates ! (mid-1))) -- only first
          mkAverage (lo,mid,hi) = (d2 / d, d1 / d, sqrt (d1*d2/d))
              where lowDate | lo > 0    = dates!(lo-1)
                            | otherwise = 0.0
                    d  = dates!(hi-1)  - lowDate
                    d1 = dates!(mid-1) - lowDate
                    d2 = dates!(hi-1)  - dates!(mid-1)

----------------------------------------------------------------------------
-- we could import BrownianBridges, to which we actually want to add code, but
-- this would require its functions to be generic (in SpecReal, not very
-- "spec"). Therefore we copy the code for the generic version here:

brownian_bridge_gen :: Pricing_Data RealE -> V.Vector RealE 
                    -> V.Vector (V.Vector RealE)
brownian_bridge_gen conf@Pricing_Data{..} =
     arrangePerDate . V.map (mkBBridge conf) . divideInto md_dim

arrangePerDate = transpose

transpose :: V.Vector (V.Vector RealE) 
          -> V.Vector (V.Vector RealE)
transpose vs = let l = V.length (V.head vs)
               in V.fromList [V.map (V.! k) vs | k <- [0..l-1] ]

-- divides into n lists, assuming bounds many elements are present
-- (round-robin distribution inefficient! )
divideInto :: Int -> V.Vector RealE -> V.Vector (V.Vector RealE)
-- correct, round-robin distribution (using backpermute). Inefficient!
-- divideInto n xs = let l  = V.length xs `div` n
--                       is = [ V.enumFromStepN i n l | i <- [0..n-1]]
--                   in V.fromList (map (V.backpermute xs) is)
-- reordered: using chunking instead of round-robin
-- divideInto n xs = let pickL start = V.slice (start*l) l xs
--                       l = V.length xs `div` n
--                   in V.fromList (map pickL [0,1..n-1])
-- Error introduced: around 0.35 (both values)
-- version using transpose to re-establish the right order
divideInto n xs = let pickL start = V.slice (start*n) n xs
                      l = V.length xs `div` n
                  in transpose (V.fromList (map pickL [0,1..l-1]))

-- TODO program a version which transforms a whole vector of vectors at a time
-- (eliminating the two transposition functions above)
mkBBridge :: Pricing_Data RealE -> V.Vector RealE -> V.Vector RealE
mkBBridge Pricing_Data{..} xs = runST mkBBridgeST
    where mkBBridgeST :: ST s (V.Vector RealE)
          mkBBridgeST = do v <- VM.new md_nb_path_dates
                           res <- fillRec 0 v
                           V.unsafeFreeze res
          fillRec n v | n == md_nb_path_dates = return v
                      | n == 0 = do VM.write v (bb_bi V.! 0-1) (bb_sd V.! 0 * V.head xs)
                                    fillRec (n+1) v
                      | otherwise = do
                          let lb  = bb_li V.! n - 1
                              ub  = bb_ri V.! n - 1
                              mid = bb_bi V.! n - 1
                              zi  = xs V.! n
                          wk <- VM.read  v ub
                          let tmp = bb_rw V.! n * wk + bb_sd V.! n * zi
                          if lb == -1 then VM.write v mid tmp
                                      else do z <- VM.read v lb
                                              VM.write v mid (tmp+z*bb_lw V.! n)
                          fillRec (n+1) v

----------------------------------------------------------------------------
-- generate code:

bbridge_codegen :: Int -> Int -> (String,String)
bbridge_codegen dim n_dates 
    = (guard, unlines (header:target:whereClause:listrows))
 where name  = dimS ++ 'x':dateS
       dimS  = show dim
       dateS = show n_dates
       guard = unwords 
               ("-- insert guard for":name:"bridge\n":
                " | md_dim ==":dimS:"&& bb_l ==":dateS:
                ("= Just (bbs_" ++ name):"conf)":[])
       header= "bbs_" ++ name ++ " Pricing_Data{..} rnd"
       target= unwords 
               ("    = V.fromList [V.slice start":dimS:("raw_"++name):"\n":
                "            | start <- [0,":dimS:"..":dimS:"*":dateS:"-1]]":[])
       whereClause 
              = "  where raw_"++name ++ " = V.fromList ["
       space s ="                              " ++ s
       listrows = (map (space . concat . intersperse ",") -- [String]
                   . intersperse [","] 
                   . map (map (show)) -- [[String]]
                   . V.toList                 -- [[RealE]]
                   . V.map V.toList) rowVs    -- Vector [RealE]
                  ++ [space "]"]
       config = abstract_init dim n_dates
       rnd = abstract_vector "rnd" (dim*n_dates)
       rowVs = brownian_bridge_gen config rnd


main = do args <- getArgs
          let maxdim = if length args < 2 then 3 else read (args!!1)
              maxdates = if null args then 5 else read (head args)
              filename = if length args < 3 then "bridgecode.hs" else (args!!2)
          writeFile filename "-- paste into BrownianBridges.hs\n\n"
          let (guards, code) = unzip (concat [[bbridge_codegen dim dates
                                               | dim <- [1..maxdim]]
                                              | dates <- [1..maxdates]])
          appendFile filename (unlines ("-- GUARDS:":guards)
                               ++ unlines ("------------":"-- CODE:":code))

-- testing: 3 x 5 config from Lexifi worst_off contract
dates_worst_off :: [Double]
dates_worst_off = [17398800, 17924400, 18450000, 18975600, 19502640]
    -- 27 Jan, in   2013      2014       2015     2016       2017
    -- (in minutes since 1980)

today_worst_off :: Double
today_worst_off = head dates_worst_off - 366*1440 -- 2012-01-27 (leap year)

conf_worst_off = bbridgeConf 3 (today_worst_off, dates_worst_off)
