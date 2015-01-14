{-# LANGUAGE GADTs #-}
module CodeGen.Utils where

import qualified Data.Text as T
import Data.List
import Data.List.Split
import Text.Regex.Posix
import Contract.Expr
import Contract.Type
import Contract

daysToDates startDate = map ((flip addDays) startDate)

--extracting transfer days

transfDays c = sort $ nub $ tDays c 0
mTransfDates (startDate, c) = daysToDates startDate $ transfDays c 

tDays (Scale e c) t = tDays c t
tDays (Transl t' c) t = tDays c (t' + t)
tDays (CheckWithin e t' c1 c2) t | t' == 0 = tDays c1 t ++ tDays c2 t
                                 | otherwise = t : tDays (CheckWithin e (t'-1) c1 c2) (t+1)
tDays (TransfOne _ _ _) t = [t]
tDays (If _ c1 c2) t = tDays c1 t ++ tDays c2 t
tDays (Both c1 c2) t = tDays c1 t ++ tDays c2 t
tDays Zero _ = []

--extracting observable days
obsDays c = sort $ nub $ oDays c 0
mObsDates (startDate, c) = daysToDates startDate $ obsDays c

oDays (Scale e c) t = oDaysE e t ++ oDays c t
oDays (Transl t' c) t = oDays c (t' + t)
oDays (If e c1 c2) t = oDaysE e t ++ oDays c1 t ++ oDays c2 t
oDays (CheckWithin e t' c1 c2) t = concat (map (oDaysE e) [t .. t+t']) ++ oDays c1 t ++ oDays c2 t
oDays (TransfOne _ _ _) t = []                
oDays Zero _ = []

oDaysE :: Expr a -> Int -> [Int]
oDaysE (Arith _ e1 e2) t = oDaysE e1 t ++ oDaysE e2 t  
oDaysE (Less e1 e2) t = oDaysE e1 t ++ oDaysE e2 t
oDaysE (Or e1 e2) t = oDaysE e1 t ++ oDaysE e2 t
oDaysE (Not e) t = oDaysE e t
oDaysE (R rLit) t = []
oDaysE (Obs (_, t')) t = [t' + t]

observables = sort . nub . getObs

getObs (Scale e c) = getObsE e ++ getObs c
getObs (Transl t' c) = getObs c
getObs (If e c1 c2) = getObsE e ++ getObs c1 ++ getObs c2
getObs (CheckWithin e _ c1 c2) = getObsE e ++ getObs c1 ++ getObs c2
getObs (TransfOne _ _ _) = []                
getObs Zero = []

getObsE :: Expr a -> [String]
getObsE (Arith _ e1 e2) = getObsE e1 ++ getObsE e2
getObsE (Less e1 e2) = getObsE e1 ++ getObsE e2
getObsE (Or e1 e2) = getObsE e1 ++ getObsE e2
getObsE (Not e) = getObsE e
getObsE (R rLit) = []
getObsE (Obs (n, _)) = [n]

fromManaged (_, c) = c

-- templates-like replacing
replace this that = intercalate that . splitOn this
replaceLabel label that = replace ("{|" ++ label ++ "|}") that
replaceLabels context = replLabs context
  where
    replLabs [] templ = templ
    replLabs ((l,that):ls) templ = replLabs ls (replace ("{|" ++ l ++ "|}") that templ)

-- some helpers for pretty-printing
inParens s = "(" ++ s ++ ")"
inCurlBr s = "{" ++ s ++ "}"
inSqBr s = "[" ++ s ++ "]"
newLn s = "\n" ++ s
inBlock = newLn . inCurlBr . newLn
commaSeparated = intercalate ", "
surroundBy c s = c ++ s ++ c
spaced = surroundBy " "

-- parsing output

parseOut :: String -> [Double]
parseOut s = map (read . T.unpack . T.strip) $ T.splitOn (T.pack ",") $ T.pack listStr
  where (_,_,_,[listStr]) = s =~ "\\[(.*?)\\]" :: (String, String, String, [String])
