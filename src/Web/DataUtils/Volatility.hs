{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module DataUtils.Volatility
    (close2Close, parkinson, rogersSatchell, garmannKlass,
     garmannKlassYZ, yangZhang, logReturn,getModel,test)
        where
import Data.Time
import qualified Data.Map as M
import GHC.Generics
import Data.Typeable
import TypeClass
import Utils
import Data
import Data.Maybe
import DataProviders.Common
import DataProviders.Data
import qualified Data.Text as T 
import PersistentData
import Fields
import Contract.Date
import qualified DataProviders.Database as Database
import Database.Persist
import Control.Monad.Trans
import Control.Monad (when)
--import qualified Test.HUnit as U

--type volatility = "close" | "parkinson" | "rogers-satchell" | "garmann-klass" | "garmann-klassYZ" | "yang-zhang"
volmodels = ["close", "parkinson", "rogers-satchell", "garmann-klass", "garmann-klassYZ", "yang-zhang"]

volatility :: RawVolatilityModel -> [RawQuotesExt] -> [RawVolatility]
volatility _ [] = []
volatility (name,annN,period,from,to) quotes = do
   case name of
      "close"           -> do 
                             let chg = generateCloseList (period-1) $ generateCloseChangeList quotes
                             volatilityLoop (name,annN,(period-1),from,to) quotes chg
      "parkinson"       -> do
                             let chg = generateParkinsonChangeList quotes
                             volatilityLoop (name,annN,period,from,to) quotes chg
      "rogers-satchell" -> do
                             let chg = generateRSChangeList quotes
                             volatilityLoop (name,annN,period,from,to) quotes chg
      "garmann-klass"   -> do
                             let chg = generateGKChangeList quotes
                             volatilityLoop (name,annN,period,from,to) quotes chg                    
      "garmann-klassYZ" -> do
                             let chg = generateGKYZChangeList quotes
                             volatilityLoop (name,annN,period,from,to) quotes chg                                                     
      _                 -> []
   

volatilityLoop :: RawVolatilityModel -> [RawQuotesExt] -> [] Double -> [RawVolatility]
volatilityLoop _ [] _ = []
volatilityLoop _ _ [] = []
volatilityLoop (name,annN,period,from,to) (raw:rawlst) (chg:chglst) = do
   let split = splitChanges period (chg:chglst)
   if length split >= period
   then do
     let lstlngth = fromIntegral $ length split
     let split_sum = foldr (+) 0 split
     case name of
        "close"           -> 
           (setVolatility raw $ annualize $ sqrt $ (1/(lstlngth-1)) * split_sum)
              :volatilityLoop (name,annN,period,from,to) rawlst chglst
        "parkinson"       ->
           (setVolatility raw $ annualize $ sqrt $ (1/(4 * lstlngth * log 2)) * split_sum)
              :volatilityLoop (name,annN,period,from,to) rawlst chglst
        "rogers-satchell" ->
           (setVolatility raw $ annualize $ sqrt $ (1/lstlngth) * split_sum)
              :volatilityLoop (name,annN,period,from,to) rawlst chglst
        "garmann-klass"   -> 
           (setVolatility raw $ annualize $ sqrt $ (1/lstlngth) * split_sum)
              :volatilityLoop (name,annN,period,from,to) rawlst chglst
        "garmann-klassYZ" -> 
           (setVolatility raw $ annualize $ sqrt $ (1/lstlngth) * split_sum)
              :volatilityLoop (name,annN,period,from,to) rawlst chglst
   else []

   
close2Close :: Int -> [RawQuotesExt] -> [RawVolatility]
close2Close 0 _ = []
close2Close _ [] = []
close2Close period (q:xs) = do
  let chg = generateCloseList (period-1) $ generateCloseChangeList (q:xs)
  close2CloseLoop (period-1) (q:xs) chg

logReturn :: [RawQuotesExt] -> [RawVolatility]
logReturn [] = []
logReturn (q:xs) = do
  let chg = generateCloseChangeList (q:xs)
  logReturnLoop (q:xs) chg

logReturnLoop :: [RawQuotesExt] -> [] Double -> [RawVolatility]
logReturnLoop _ [] = []
logReturnLoop [] _ = []
logReturnLoop (raw:rawlst) (chg:chglst) = do
   let day_one = getCloseingPrice raw
   let day_two = getCloseingPrice $ getQuote rawlst
   let vol = calculateChange day_one day_two
   let new_vol = setCloseValue raw vol
   new_vol:logReturnLoop rawlst chglst

parkinson :: Int -> [RawQuotesExt] -> [RawVolatility]
parkinson 0 _ = []
parkinson _ [] = []
parkinson period (raw:rawlst) = do
   let chg = generateParkinsonChangeList (raw:rawlst)
   parkinsonLoop period (raw:rawlst) chg
   
rogersSatchell :: Int -> [RawQuotesExt] -> [RawVolatility]
rogersSatchell 0 _ = []
rogersSatchell _ [] = []
rogersSatchell period (raw:rawlst) = do
   let chg = generateRSChangeList (raw:rawlst)
   rogersSatchellLoop period (raw:rawlst) chg

garmannKlass :: Int -> [RawQuotesExt] -> [RawVolatility]
garmannKlass 0 _ = []
garmannKlass _ [] = []
garmannKlass period (raw:rawlst) = do
   let chg = generateGKChangeList (raw:rawlst)
   garmannKlassLoop period (raw:rawlst) chg

garmannKlassYZ :: Int -> [RawQuotesExt] -> [RawVolatility]
garmannKlassYZ 0 _ = []
garmannKlassYZ _ [] = []
garmannKlassYZ period (raw:rawlst) = do
   let chg = generateGKYZChangeList (raw:rawlst)
   garmannKlassLoop period (raw:rawlst) chg
   
yangZhang :: Int -> [RawQuotesExt] -> [RawVolatility]
yangZhang 0 _ = []
yangZhang _ [] = []
yangZhang period (raw:rawlst) = do
   let rs_chg = generateRSChangeList (raw:rawlst)
   let vol_rs = rogersSatchellRaw period (raw:rawlst) rs_chg
   let overnight_chg = generateMuChangeList period $ generateOvernightChangeList(raw:rawlst)
   let o2close_chg = generateMuChangeList period $ generateOpen2CloseChangeList(raw:rawlst)
   yangZhangLoop period (raw:rawlst) overnight_chg o2close_chg vol_rs 

-- The function calculates the volatility according to the
-- close-to-close model.
-- Input: period of time - eg 10, 20 or 30 days volatility
-- Input: A set of Quotes - only closing prices are used
-- Input: A list of changes day-to-day of the portfolio
-- Output: A list of quoutes with an calculated volatility
close2CloseLoop :: Int -> [RawQuotesExt] -> [] Double -> [RawVolatility]
close2CloseLoop _ [] _ = []
close2CloseLoop _ _ [] = []
close2CloseLoop period (raw:rawlst) (chg:chglst) = do
   let split = splitChanges period (chg:chglst)
   if length split >= period
   then do
     let lstlngth = fromIntegral $ length split
     let sumdiff = foldr (+) 0 split
     let sums = sqrt $ (1/(lstlngth-1)) * sumdiff
     let vol = annualize sums
     let new_vol = setCloseValue raw vol
     new_vol:close2CloseLoop period rawlst chglst
   else []

parkinsonLoop :: Int -> [RawQuotesExt] -> [] Double -> [RawVolatility]
parkinsonLoop _ [] _ = []
parkinsonLoop _ _ [] = []
parkinsonLoop period (raw:rawlst) (chg:chglst) = do
   let split = splitChanges period (chg:chglst)
   if length split >= period
   then do
     let split_sum = foldr (+) 0 split
     let periodD = fromIntegral $ period
     let lndel = 1/(4 * periodD * log 2)
     let prod = annualize $ sqrt $ lndel * split_sum
     let new_vol = setVolatility raw prod
     new_vol:parkinsonLoop period rawlst chglst
   else []

rogersSatchellLoop :: Int -> [RawQuotesExt] -> [] Double -> [RawVolatility]
rogersSatchellLoop _ [] _ = []
rogersSatchellLoop _ _ [] = []
rogersSatchellLoop period (raw:rawlst) (chg:chglst) = do
   let split = splitChanges period (chg:chglst)
   if length split >= period
   then do
     let split_sum = foldr (+) 0 split
     let periodD = fromIntegral $ period
     let prod = annualize $ sqrt $ (1/periodD) * split_sum
     let new_vol = setVolatility raw prod
     new_vol:rogersSatchellLoop period rawlst chglst
   else []
 
rogersSatchellRaw :: Int -> [RawQuotesExt] -> [] Double -> [RawVolatility]
rogersSatchellRaw _ [] _ = []
rogersSatchellRaw _ _ [] = []
rogersSatchellRaw period (raw:rawlst) (chg:chglst) = do
   let split = splitChanges period (chg:chglst)
   if length split >= period
   then do
     let split = splitChanges period (chg:chglst)
     let split_sum = foldr (+) 0 split
     let periodD = fromIntegral $ period
     let prod = (1/periodD) * split_sum
     let new_vol = setVolatility raw prod
     new_vol:rogersSatchellRaw period rawlst chglst
   else []
   
garmannKlassLoop :: Int -> [RawQuotesExt] -> [] Double -> [RawVolatility]
garmannKlassLoop _ [] _ = []
garmannKlassLoop _ _ [] = []
garmannKlassLoop period (raw:rawlst) (chg:chglst) = do
   let split = splitChanges period (chg:chglst)
   if length split >= period
   then do
      let split_sum = foldr (+) 0 split
      let periodD = fromIntegral $ period
      let res = (1/periodD) * split_sum
      let prod = annualize $ sqrt res
      let new_vol = setVolatility raw prod
      new_vol:garmannKlassLoop period rawlst chglst
   else []
   

-- This differs
-- The functions calculates the YangZhang volatility model
-- The input is the period, the raw quoutes and
-- 1: The overnight volatility calulations as ln (o_i/c_i-1)
-- 2: The open to close volatility calculations as ln(c_i/o_i)
-- 3: RogersSatchell volatilities
-- Output is Yang Zhang volatility
yangZhangLoop :: Int -> [RawQuotesExt] -> [] Double -> [] Double -> [RawVolatility] -> [RawVolatility]
yangZhangLoop _ [] _ _ _ = []
yangZhangLoop _ _ [] _ _ = []
yangZhangLoop _ _ _ [] _ = []
yangZhangLoop _ _ _ _ [] = []
yangZhangLoop period (raw:rawlst) (overnight:olst) (open2close:clist) ((u,d,rsv):rs) = do
   let n = fromIntegral $ period
   let k = 0.34 / (1.34 + ((n+1)/(n-1)))
   -- Overnight volatility
   let split_overnight  = splitChanges period (overnight:olst)
   let overnight_vol = (1/(n-1)) * (foldr (+) 0 split_overnight)
   -- Open to close volatility
   let split_open2close = splitChanges period (open2close:clist)
   let o2close_vol = (1/(n-1)) * (foldr (+) 0 $ split_open2close)
   let result = overnight_vol + (k * o2close_vol) + (1-k)*rsv
   let prod = annualize $ sqrt result
   let new_vol = setVolatility raw prod
   new_vol:yangZhangLoop period rawlst olst clist rs
  

-- The function receives a list as argument and calculates the
-- relative change from n_0 to n_1 - The function returns a new list
generateCloseChangeList :: [RawQuotesExt] -> [] Double
generateCloseChangeList [] = []
generateCloseChangeList ((u,d,o,l,h,c):[]) = []
generateCloseChangeList ((u,d,o,l,h,c):xs) = do
   let day_one = c
   let day_two = getCloseingPrice $ getQuote xs
   calculateChange day_one day_two:generateCloseChangeList xs
   
generateCloseList :: Int -> [] Double -> [] Double
generateCloseList _ [] = []
generateCloseList period (x:xs) = do
   let split = splitChanges period (x:xs)
   if length split >= period
     then do   
       let mean = closeAverage period split
       (x-mean)^2:generateCloseList period xs
   else []
   
generateParkinsonChangeList :: [RawQuotesExt] -> [] Double
generateParkinsonChangeList [] = []
generateParkinsonChangeList ((u,d,o,l,h,c):xs) = (log(h/l))^2:generateParkinsonChangeList xs

generateRSChangeList :: [RawQuotesExt] -> [] Double
generateRSChangeList [] = []
generateRSChangeList ((u,d,o,l,h,c):xs) = 
  ((log(h/c)*log(h/o))+(log(l/c)*log(l/o))):generateRSChangeList xs

generateGKChangeList :: [RawQuotesExt] -> [] Double
generateGKChangeList [] = []
generateGKChangeList ((u,d,o,l,h,c):xs) = do
   let day_one = o
   let day_two = getCloseingPrice $ getQuote xs
   let exp2 = 0.5*((log(h/l))^2)
   let exp3 = (2*log(2)-1)*((log(c/o))^2)
   let vol = exp2 - exp3
   vol:generateGKChangeList xs

generateGKYZChangeList :: [RawQuotesExt] -> [] Double
generateGKYZChangeList [] = []
generateGKYZChangeList ((u,d,o,l,h,c):xs) = do
   let day_one = o
   let day_two = getCloseingPrice $ getQuote xs
   let exp1 = (calculateChange day_one day_two)^2
   let exp2 = 0.5*((log(h/l))^2)
   let exp3 = (2*log(2)-1)*((log(c/o))^2)
   let vol = exp1 + exp2 - exp3
   vol:generateGKYZChangeList xs

generateOvernightChangeList :: [RawQuotesExt] -> [] Double
generateOvernightChangeList [] = []
generateOvernightChangeList ((u,d,o,l,h,c):xs) = do
   let day_one = o
   let day_two = getCloseingPrice $ getQuote xs
   calculateChange day_one day_two:generateOvernightChangeList xs   

generateOpen2CloseChangeList :: [RawQuotesExt] -> [] Double
generateOpen2CloseChangeList [] = []
generateOpen2CloseChangeList ((u,d,o,l,h,c):xs) = do
   let day_one = c
   let day_two = o
   calculateChange day_one day_two:generateOpen2CloseChangeList xs   

generateMuChangeList :: Int -> [] Double -> [] Double
generateMuChangeList _ [] = []
generateMuChangeList 0 _ = []
generateMuChangeList period (x:xs) = do
   let periodD = fromIntegral period
   let split = splitChanges period (x:xs)
   let res = (1/periodD) * (foldr (+) 0 split)
   (x - res)^2:generateMuChangeList period xs
      
  
setVolatility :: RawQuotesExt -> Double -> RawVolatility
setVolatility (u,d,o,l,h,c) vol = (u,d,vol)

splitChanges :: Int -> [] Double -> [] Double
splitChanges _ [] = []
splitChanges i lst = take i lst

-- The function sums the quotes in a list
sumClose :: [RawQuotesExt] -> Double
sumClose ((u,d,o,l,h,c):xs) = c + sumClose xs
sumClose [] = 0

closeAverage :: Int -> [] Double -> Double
closeAverage 0 _  = 0
closeAverage cnt lst = do
  let lgth = fromIntegral cnt
  let nlst = take cnt lst
  (foldr (+) 0 nlst)/lgth

-- The function returns the simple average of numbers
simpleAverage :: Double -> [] Double -> Double
simpleAverage 0 _  = 0
simpleAverage cnt lst = (foldr (+) 0 lst)/cnt

-- The function substracts the mean from each number in the list
-- and squares the result
subMean :: Double -> [] Double -> [] Double
subMean mean (x:xs) = ((x-mean)^2):subMean mean xs
subMean mean [] = []

sumCloseNMinus1 :: [] Double -> Double
sumCloseNMinus1 [] = 0
sumCloseNMinus1 (x:[]) = 0
sumCloseNMinus1 (x:xs) = x + sumCloseNMinus1 xs
     

standardDev :: [] Double -> Double
standardDev lst = do
  let lstlngth = (fromIntegral $ length lst)-1
  --let mean = simpleAverage lstlngth lst
  let mean = closeAverage lstlngth lst
  let diff_lst = subMean mean lst
  sqrt $ closeAverage lstlngth diff_lst
  
-- The function is used for calculating the relative change in prices from day-to-day
-- It takes two arguments of type Double, day_one and day_two
calculateChange :: Double -> Double -> Double
calculateChange day_one day_two
    | day_two /=  0.0 = log (day_one/day_two)
    | otherwise = 0.0

-- Annualizes the result
-- If period is dayli then times sqrt(252) as there are 252 trading days
annualize :: Double -> Double
annualize n = n * sqrt(252)

getQuote :: [RawQuotesExt] -> RawQuotesExt
getQuote [] = ("", getToday,0,0,0,0)
getQuote (x:xs) = x
  
getCloseingPrice :: RawQuotesExt -> Double
getCloseingPrice (u,d,o,l,h,c) = c

getOpenPrice :: RawQuotesExt -> Double
getOpenPrice (u,d,o,l,h,c) = o

getQuoteDay :: RawQuotesExt -> Day
getQuoteDay (u,d,o,l,h,c) = d

setCloseValue :: RawQuotesExt -> Double -> RawVolatility
setCloseValue (u,d,o,l,h,c) vol = (u,d,vol)

getToday :: Day
getToday = contrDate2Day $ at "2016-01-01"

getModel :: String -> Int -> Day -> Day -> RawVolatilityModel
getModel m l from to = (m, 252, l, from, to)



-- ******************************* TEST FUNCTIONS *********************
-- ******************************* TEST FUNCTIONS *********************
test :: String -> String -> String -> Int -> IO [RawVolatility]
test und from to period = do
   let f = contrDate2Day $ at from
   let t = contrDate2Day $ at to
    
   quotes <- liftIO $ Database.getStoredQuotesExtPeriod und f t
   
   let closechange = generateCloseChangeList quotes
   let chg = generateCloseList (period-1) closechange 
   let c2c = close2CloseLoop (period-1) quotes chg
   --print c2c
   
   let model = ("close",252,period,f,t)
   let closOld = close2Close period quotes
   let closNew = volatility model quotes
   print $ closOld == closNew
   print "OLD"
   print closOld
   print "NEW"
   print closNew

--   let model = ("parkinson",252,period,f,t)
--   let pOld = parkinson period quotes
--   let pNew = volatility model quotes

   
--   let model = ("rogers-satchell",252,period,f,t)
--   let rsOld = rogersSatchell period quotes
--   let rsNew = volatility model quotes

   
--   let model = ("garmann-klass",252,period,f,t)
--   let gkOld = garmannKlass period quotes
--   let gkNew = volatility model quotes

   
--   let test1 = U.TestCase (U.assertEqual "Close to close" closOld closNew) 
--   let test2 = U.TestCase (U.assertEqual "Parkinson" pOld pNew) 
--   let test3 = U.TestCase (U.assertEqual "Rogers-Satchell" rsOld rsNew)
--   let test4 = U.TestCase (U.assertEqual "Garmann-Klass" gkOld gkNew)
--   let tests = U.TestList [U.TestLabel "Test1: Close to close" test1,
--                       U.TestLabel "Test2: Parkinson" test2,
--                       U.TestLabel "Test3: Rogers-Satchell" test3,
--                       U.TestLabel "Test4: Garmann-Klass" test4]
--   U.runTestTT tests
   

--    -- Generate change list
--    let chg = generateCloseList period $ generateCloseChangeList (q:xs)
--    let pchg = generateParkinsonChangeList (q:xs)
--    let rschg = generateRSChangeList (q:xs)
--    let gkchg = generateGKChangeList (q:xs)
--    let (overnight:olst) = generateMuChangeList period $ generateOvernightChangeList (q:xs)
--    let (open2close:clist) = generateMuChangeList period $generateOpen2CloseChangeList (q:xs)
--   
--    -- Calculate volatilities
--    let cm = getModel "Close to close" period f t
--    let set = close2CloseLoop period (q:xs) chg
--    i <- Database.insertVolatilityModel cm
--    Database.insertVolatilities i set
--    
--    let pm = getModel "Parkinson" period f t
--    let pk = parkinsonLoop period (q:xs) pchg
--    i <- Database.insertVolatilityModel pm
--    Database.insertVolatilities i pk
--    
--    let rm = getModel "Rogers-Satchell" period f t
--    let rs = rogersSatchellLoop period (q:xs) rschg
--    i <- Database.insertVolatilityModel rm
--    Database.insertVolatilities i rs
--    
--    let gkm = getModel "Garmann-Klass" period f t
--    let gk = garmannKlassLoop period (q:xs) gkchg
--    i <- Database.insertVolatilityModel gkm
--    Database.insertVolatilities i gk
--    
--    let gym = getModel "Garman-Klass Yang-Zhang" period f t
--    let gkyz = garmannKlassYZ period (q:xs)
--    i <- Database.insertVolatilityModel gym
--    Database.insertVolatilities i gkyz
--   
--   let ym = getModel "Yang-Zhang" period f t
--   let yz = yangZhang period quotes
--   i <- Database.insertVolatilityModel ym
--   Database.insertVolatilities i yz
   
   return closNew
   


   

   
