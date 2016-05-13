{-# LANGUAGE GADTs #-}
module Utils where

import qualified Contract.Date as D
import Contract.Type
import Contract.Expr
import Contract.ExprIO
import qualified Contract  as C

import Data.Time (Day)
import Data.Time.Format (parseTimeOrError, formatTime, defaultTimeLocale)
import Data.List
import Data.Char (toUpper)


-- remove spaces from both ends of string
trim = unwords . words


-- This looks ugly. We should get rid of Date and use Day instead.
-- Converting date defined in "contract" project to regular Haskell's Day.
contrDate2Day :: D.Date -> Day
contrDate2Day  = parseDate . D.ppDate
day2ContrDate :: Day -> D.Date
day2ContrDate = read . formatDate

parseDate = parseTimeOrError True defaultTimeLocale dateFormat
formatDate = formatTime defaultTimeLocale dateFormat

dateFormat = "%Y-%m-%d"

capFirst (x:xs) = toUpper x : xs

ppExpr :: Expr a -> String
ppExpr e = 
    case e of
           V s -> s
           I i -> show i
           R r -> show r
           B b -> show b
           Pair e1 e2 -> par (ppExpr e1 ++ "," ++ ppExpr e2)
           Fst e -> "first" ++ par (ppExpr e)
           Snd e -> "second" ++ par (ppExpr e)
           Acc f i e -> "acc" ++ par(ppFun f ++ "," ++ show i ++ "," ++ ppExpr e)
           Obs (s,off) -> s ++ "@" ++ show off
           ChosenBy (p,i) -> "Chosen by " ++ p ++ " @ " ++ show i
           Not e1 -> "not" ++ par (ppExpr e1)
           Arith op e1 e2 -> let (c,infx) = ppOp op
                             in if infx then par(ppExpr e1 ++ c ++ ppExpr e2)
                                else c ++ par (ppExpr e1 ++ ", " ++ ppExpr e2)
           Less e1 e2 -> par(ppExpr e1 ++ " < " ++ ppExpr e2)
           Equal e1 e2 -> par(ppExpr e1 ++ "==" ++ ppExpr e2)
           Or e1 e2 ->  par(ppExpr e1 ++ "||" ++ ppExpr e2)
    where ppFun (v,e) = "\\" ++ v ++ " -> " ++ ppExpr e
                  
-- | pretty-prints a contract.
ppContract :: Contract -> String
ppContract c = 
    case c of
      TransfOne c p1 p2 -> "transfOne" ++ par (show c ++ ", " ++ p1 ++ "->" ++ p2)
      Scale e c -> ppExpr e ++ " * " ++ ppContract c
      Transl i c -> "transl" ++ par (D.ppDays i ++ ", " ++ ppContract c)
      Zero -> "zero"
      Both c1 c2 -> "both" ++ par (ppContracts[c1,c2])
      If e c1 c2 -> "if " ++ ppExpr e ++ " then " ++ ppContract c1 ++ " else " ++ ppContract c2
      CheckWithin e i c1 c2 -> 
           "checkWithin" ++ par (ppExpr e ++ ", " ++ D.ppDays i ++ ", "  ++ ppContract c1 ++ ", " ++ ppContract c2)
      --   | Let(v,e,c) -> "Let" ++ par (v ++ "," ++ ppExpr e ++ "," ++ ppContract c)
    where par s = "(" ++ s ++ ")"

ppContracts [] = ""
ppContracts [c] = ppContract c
ppContracts (c:cs) = ppContract c ++ ", " ++ ppContracts cs
