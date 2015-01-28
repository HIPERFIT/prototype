{-# LANGUAGE GADTs #-}

module CodeGen.OpenclGen where

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Time as D

import System.IO

import Contract
import LexifiContracts
import Contract.Expr
import Contract.Date
import Contract.Type
import Contract.Transform
import CodeGen.Utils
import qualified Config as Conf


-- data type representing AST of some simple imperative language
data CLCode = DeclareVar String String CLCode
            | Assign String CLCode
            | IfStmt CLCode [CLCode] [CLCode]
            | FunCall String [CLCode]
            | BinOp String CLCode CLCode
            | BoolExpr2 String CLCode CLCode
            | BoolNot CLCode  
            | Lit String
            | Var String  
            deriving Show
-- (Observable_days, Transfer_days, Names_of_observables)
type GenEnv = ([Int], [Int], [String])

accAssign op name expr = Assign name $ BinOp op (Var name) expr
mulAssign = accAssign "*" 

-- transforming contracts to CLCode
genPayoffFunc c = amountDecl : body
              where
                amountDecl = DeclareVar "REAL" "amount" $ Lit "1"
                env = (obsDays c, transfDays c, observables c)
                body = genCLCode (Just c) env 0

genCLCode Nothing _ _ = []
genCLCode (Just cc) env@(ods, tds, obs) t =
  case cc of
       (TransfOne _ _ _)
          | (Just i) <- findIndex (== t) tds -> [trajInner i]
          | otherwise -> error $ "No such transfer day: " ++ show t
       (If cond b1 b2) -> [IfStmt (genCLExpr cond env t)
                                  (genCLCode (Just b1) env t)
                                  (genCLCode (Just b2) env t)]
       (Transl t' c') -> genCLCode (Just c') env (t' + t)
       (CheckWithin e t' c1 c2) -> genIfs e c1 c2 t t'
       _
          | null e -> genCLCode rest env t
          | otherwise -> (mulAssign "amount" $ genCLExpr (calcScale e) env t) : genCLCode rest env t
  where
    genIfs e c1 c2 t1 t2
      | t1 == t2  = genCLCode (Just c2) env t1
      | otherwise = [IfStmt (genCLExpr e env t1)
                     (genCLCode (Just c1) env t1)
                     (genIfs e c1 c2 (t1+1) t2)]
    (e, rest) = collectScalings [] cc

-- collecting scalings till first "if", "checkWithin" or elementary contract such as TransfOne or Zero
-- TODO Check whether it needed 
collectScalings acc (Scale e c) = collectScalings(e : acc) c
collectScalings acc rest@(Transl _ _) = (acc, Just rest)
collectScalings acc transf@(TransfOne _ _ _) = (acc, Just transf)                
collectScalings acc Zero = (acc, Nothing)
collectScalings acc rest@(If _ _ _) = (acc, Just rest)
collectScalings acc rest@(CheckWithin _ _ _ _) = (acc, Just rest)

calcScale [] = (r 1)
calcScale xs = foldl1 (*) xs                

trajInner day = FunCall "trajectory_inner"
                        [Var "num_cash_flows", Var "model_num",
                         Lit $ show day, Var "amount", Var "md_discts",
                         Var "vhat"]

genCLExpr :: Expr a -> GenEnv -> Int -> CLCode
genCLExpr (Arith Max e1 e2) env t = FunCall "fmax" [genCLExpr e1 env t, genCLExpr e2 env t]
genCLExpr (Arith Min e1 e2) env t = FunCall "fmin" [genCLExpr e1 env t, genCLExpr e2 env t]
genCLExpr (Arith Minus e1 e2) env t = BinOp "-" (genCLExpr e1 env t) (genCLExpr e2 env t)
genCLExpr (Arith Times e1 e2) env t = BinOp "*" (genCLExpr e1 env t) (genCLExpr e2 env t)
genCLExpr (Arith Div e1 e2) env t = BinOp "/" (genCLExpr e1 env t) (genCLExpr e2 env t)
genCLExpr (Less e1 e2) env t = BoolExpr2 "<" (genCLExpr e1 env t) (genCLExpr e2 env t)
genCLExpr (Or e1 e2) env t = BoolExpr2 "||" (genCLExpr e1 env t) (genCLExpr e2 env t)
genCLExpr (Not e) env t = BoolNot $ genCLExpr e env t
genCLExpr (R rLit) env t = Lit $ show rLit
genCLExpr (Obs (n, t')) (ods, _, obs) t
           | (Just i) <- findIndex (== (t' + t)) ods, (Just j) <- findIndex (== n) obs = underlyings (i, j)
           | otherwise = error $ "No value in environment"
           where
                 underlyings (i,j) = FunCall "underlyings" [Lit $ show i, Lit $ show j]

-- pretty-printing OpenCL-like code

ppCLSeq p = (intercalate ";\n" $ map ppCLCode p) ++ ";\n"

ppCLCode (DeclareVar ty name val) = ty ++ " " ++ name ++ surroundBy " " "=" ++ ppCLCode val
ppCLCode (Assign name val) = name ++ spaced "=" ++ ppCLCode val
ppCLCode (FunCall name args) = name ++ inParens (commaSeparated $ map ppCLCode args)
ppCLCode (BinOp op e1 e2) = inParens $ ppCLCode e1 ++ surroundBy " " op ++ ppCLCode e2
ppCLCode (BoolExpr2 op e1 e2) = inParens $ ppCLCode e1 ++ surroundBy " " op ++ ppCLCode e2
ppCLCode (BoolNot e) = "!" ++ (inParens $ ppCLCode e)
ppCLCode (IfStmt cond b1 b2) = "if" ++ spaced (inParens $ ppCLCode cond) ++
                               (inBlock $ ppCLSeq b1) ++ (if (not $ null b2) then elseBranch else "") 
                               where
                                 elseBranch = spaced "else" ++ (inBlock $ ppCLSeq b2)
ppCLCode (Lit s) = s
ppCLCode (Var v) = v

writeOpenCL p fileName=
  do
    template <- readFile "proto/templ/ContractTemplate.cl"
    writeFile (Conf.genCodePath ++ fileName) (replaceLabel "CODE" p template)

renderOpenCL p =
  do
    template <- readFile "proto/templ/KernelTemplate.cl"
    return (replaceLabel "CODE" p template)
