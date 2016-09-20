{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module DataUtils.Correlation (pearsonCorrelation, pearson, covariance,covar, test) where
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
--import Data.Vector
import qualified DataProviders.Database as Database
import Database.Persist
import Control.Monad.Trans
import Control.Monad (when)



-- Calculates the pearson correlation with a RawVolatility input
pearsonCorrelation :: [RawVolatility] -> [RawVolatility] -> Double
pearsonCorrelation x y = do
  let lstx = extractVolatilities x
  let lsty = extractVolatilities y
  pearson lstx lsty
  
   
extractVolatilities :: [RawVolatility] -> [] Double
extractVolatilities [] = []
extractVolatilities ((u,d,p):xs) = p:extractVolatilities xs

pearson :: [] Double -> [] Double -> Double
pearson [] [] = 0
pearson _ [] = 0
pearson [] _ = 0
pearson (x:xs) (y:ys) = do
  let cov = covariance (x:xs) (y:ys)
  let stdevx = standardDev (x:xs)
  let stdevy = standardDev (y:ys)
  cov / (stdevx*stdevy)
  
  
covar :: [RawVolatility] -> [RawVolatility] -> Double
covar [] _ = 0
covar _ [] = 0
covar x y = do
  let lstx = extractVolatilities x
  let lsty = extractVolatilities y
  covariance lstx lsty
  
covariance :: [] Double -> [] Double -> Double
covariance [] _ = 0
covariance _ [] = 0
covariance (x:xs) (y:ys) = do
  -- get average for both datasets
  let x_lngth = fromIntegral $ length (x:xs)
  let y_lngth = fromIntegral $ length (y:ys)
  let avgx =  (foldr (+) 0 (x:xs))/x_lngth  
  let avgy =  (foldr (+) 0 (y:ys))/y_lngth 
  
  -- then get the sum of (x_i - avgx)*(y_i - avgy)
  let nominator_lst = subMean avgx avgy (x:xs) (y:ys)
  let nominator_sum = (foldr (+) 0 nominator_lst)
  let denom = x_lngth - 1
  (1/denom) * nominator_sum

standardDev :: [] Double -> Double
standardDev [] = 0
standardDev (x:xs) = do
   let lngth = fromIntegral $ length (x:xs)
   let mean = (foldr (+) 0 (x:xs))/lngth
   let nominator = subMeanSquare mean (x:xs)
   sqrt $ (nominator/(lngth-1))

-- TODO: Use foldl
-- The function substracts the mean from each number in the list
-- and squares the result
subMean :: Double -> Double -> [] Double -> [] Double -> [] Double
subMean _ _ _ [] = []
subMean _ _ [] _ = [] 
subMean avg_x avg_y (x:xs) (y:ys) = ((x-avg_x)*(y-avg_y)):subMean avg_x avg_y xs ys

subMeanSquare :: Double -> [] Double -> Double
subMeanSquare _ [] = 0
subMeanSquare mean (x:xs) = ((x-mean)^2) + subMeanSquare mean xs


-- ******************************* TEST FUNCTIONS *********************
-- ******************************* TEST FUNCTIONS *********************
test :: [RawVolatility] -> [RawVolatility] -> IO Double
test (q:xs) (y:ys) = do
    let vol1 = extractVolatilities (q:xs)
    let vol2 = extractVolatilities (y:ys)
    let stdev1 = standardDev vol1
    let stdev2 = standardDev vol2
    
    
    print "Test: Correlation - Stddev"
    print stdev1
    print stdev2
    return stdev1
    
 
   