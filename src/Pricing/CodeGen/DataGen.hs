{-# LANGUAGE DataKinds #-}
module CodeGen.DataGen where

import Numeric
import System.IO
import Data.List
import qualified Data.Map as Map
import Data.Ord
import qualified Numeric.LinearAlgebra.HMatrix as M
import GHC.TypeLits
import qualified Data.Vector as V

import CodeGen.BBridgeGen
import CodeGen.Utils
import qualified Config as Conf
import Contract.Date
import Contract
import LexifiContracts

type MarketData = ([Corr], [Quotes])

type SobolDirVects = [String]

data Corr = Corr (String, String) Double
          deriving Show
data Quotes = Quotes String [(Date, Double)]
            deriving Show

data Model = BS String [(Date, Double, Double)]
           deriving Show

type ModelData = [Model]

data DiscModel = ConstDisc Double 
               | CustomDisc [(Int,Double)]
                 deriving Show

data ContractMeta = CM
    { underlyings :: [String]
    , startDate   :: Date 
    , obsDates    :: [Date]
    , transfDates :: [Date]
    , allDates    :: [Date] -- observable & transfer dates
    }
                    deriving Show

data DataConf  = DataConf { monteCarloIter :: Int }

genInput :: DataConf -> [(DiscModel, ModelData, MarketData)] -> SobolDirVects -> ContractMeta -> [(String,String)]
genInput dConf ds sob cMeta = context
  where
    (discMs, ms, mds) = unzip3 ds
    (sourceCorrs, quotes) = unzip mds
    numDates = length $ allDates cMeta
    numUnder = length $ underlyings cMeta
    numMods = length ds
    summary = genSummary (monteCarloIter dConf) numMods numDates numUnder
    dayOffsets = map (dateDiff (startDate cMeta)) (transfDates cMeta)
    discounts discM = map (discount discM) dayOffsets
    discs = map discounts discMs
    bbMeta = ppBBMeta $ genBBConf numUnder (startDate cMeta) (allDates cMeta)
    modelData = ppModelData $ unzip $ map prepareModelData $ map (map (filterModelData (allDates cMeta))) ms
    stPrice = show $ map (startPrice (startDate cMeta)) quotes
    corrs = ppCorrs $ map (cholesky . (corrMatr (underlyings cMeta))) sourceCorrs
    context = [("SUMMARY", summary), ("DIRVECT", ppDirVects sob),
               ("CORR", corrs), ("MODELDATA", modelData), ("STARTPRICE", stPrice),
               ("DETVALS", inSqBr $ commaSeparated $ replicate numMods "[]"), 
               ("DISCOUNTS", show discs), ("BBMETA", bbMeta)]

filterModelData dates (BS und ms) = BS und $ filter f ms
    where
      f m@(date, v, d) = date `elem` dates

genSummary mcIter numMods numDates numUnder = 
    intercalate "\n" $ map show [contrNum, mcIter, numDates, numUnder, numMods, sobolBitLength] 
  where
    -- some magic numbers from Medium data.
    contrNum = 2
    sobolBitLength = 30

extractMeta mc@(d,c) = CM { underlyings = observables c
                          , startDate = d
                          , obsDates = oDates
                          , transfDates = tDates
                          , allDates = dates}
  where
    oDates = mObsDates mc
    tDates = mTransfDates mc
    dates = nub $ sort $ oDates ++ tDates

corrMatr us cs = (M.matrix size corrMatr) + M.ident size
  where
    size = length us
    source = cs ++ map permute cs
    ps = [(x,y) | x <- us, y <- us]
    corrMatr = map (f source) ps 
    f xs x = case (find (matchPair x) xs) of
               Just (Corr _ v) -> v
               Nothing -> 0
    matchPair p (Corr p' v) = p == p'
    permute (Corr (u1, u2) v) = Corr (u2,u1) v

cholesky m = cholM + strLTriang
   where
     cholM = M.chol m -- triangular matrix, but we need symmetric matrix ...
     -- making strictly lower triangular matrix (zeros along diagonal)
     -- that we can add to cholM to obtain symmetric one
     strLTriang = M.tr (cholM - (M.diag $ M.takeDiag cholM)) 


prepareModelData md = (vols, drifts)
  where
    sorted = sortBy (comparing (\(BS n _) -> n)) md
    vols = Data.List.transpose $ map (fst . f) sorted
    drifts = Data.List.transpose $ map (snd . f) sorted
    f (BS _ xs) = case (unzip3 xs) of
                  (_, vols, drifts) -> (vols, drifts)

startPrice date qs = map (getPrice date) $ sortBy (comparing (\(Quotes n _) -> n)) qs

getPrice date (Quotes _ qs) = case (find ((== date) . fst) qs) of
                              Just (_,v) -> v
                              Nothing -> error ("No quote for the date " ++ show date)

discount :: DiscModel -> Int -> Double
discount (ConstDisc r) t = exp (-r * yearFraction)
         where
           yearFraction = fromIntegral t / 365
discount (CustomDisc xs) t = (Map.fromList xs) Map.! t

y1980 = read "1980-01-01"
toMinutesFrom1980 :: Date -> Double 
toMinutesFrom1980 d = fromIntegral $ (dateDiff y1980 d) * 24 * 60

datesForBB sd ds = (firstDate, dates)
           where
             firstDate = fromIntegral $ dateDiff y1980 sd * 24 * 60
             dates = map toMinutesFrom1980 ds

genBBConf numUnder startDate dates = bbridgeConf numUnder $ datesForBB startDate dates 

-- TODO: possibly, we should read sobol data lazily
generateData dConf ds contr = 
  do
    vects <- readFile "./src/Pricing/CodeGen/sobol_vect.data"
    let cm = extractMeta contr
        sob = take ((length $ underlyings cm) * (length $ allDates cm)) (lines vects)
        context = genInput dConf ds sob cm
    template <- readFile "./src/Pricing/templ/InputTemplate.data"
    return (replaceLabels context template)


-- Pretty-printing data
ppModelData (vols, drifts) = (sqBrBlock $ intercalate ",\n" $ map ppRows vols) ++ "\n\n" ++ 
                             (sqBrBlock $ intercalate ",\n" $ map ppRows drifts)
ppRows = sqBrBlock . (intercalate ",\n") . (map (ppList . (map ppDouble')))
ppList = inSqBr . commaSeparated 

ppDouble p d = showFFloat (Just p) d ""

ppDouble' = ppDouble 16 

ppDirVects vs = sqBrBlock $ intercalate ",\n" (map inSqBr vs)

ppBBMeta bbMeta = sqBrBlock (intercalate ",\n" (map (show . V.toList) [bb_bi bbMeta, bb_li bbMeta, bb_ri bbMeta])) ++ "\n\n"
                  ++ sqBrBlock (intercalate ",\n" (map (show . V.toList) [bb_sd bbMeta, bb_lw bbMeta, bb_rw bbMeta]))

ppCorr corrM = inSqBr $ intercalate ",\n" $ map (ppList . map (ppDouble 7)) $ M.toLists corrM
ppCorrs cs = sqBrBlock $ intercalate ",\n" $ map ppCorr cs

sqBrBlock = inSqBr . surroundBy "\n"

