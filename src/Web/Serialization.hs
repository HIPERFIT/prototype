{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs, FlexibleInstances, StandaloneDeriving #-}
module Serialization where

import Contract.Expr
import Contract.Type
import Data.Word
import Data.Data
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Read

deriving instance Read AOp
deriving instance Read Contract

instance Read RealE where
    readPrec = parens $ 
                   do
                      Ident "R" <- lexP
                      v <- step readPrec
                      return $ R v
               +++ do
                     Ident "Arith" <- lexP
                     op <- step readPrec
                     v1 <- step readPrec
                     v2 <- step readPrec
                     return $ Arith op v1 v2
                   
               +++ do
                     Ident "Obs" <- lexP
                     v <- step readPrec
                     return $ Obs v

instance Read BoolE where
    readPrec = parens $
                   do
                     Ident "B" <- lexP
                     v <- step readPrec
                     return $ B v
               +++ do
                     Ident "Less" <- lexP
                     v1 <- step readPrec
                     v2 <- step readPrec
                     return $ Less (v1 :: RealE) (v2 :: RealE)
               +++ do
                     Ident "Equal" <- lexP
                     v1 <- step readPrec
                     v2 <- step readPrec
                     return $ Less (v1 :: RealE) (v2 :: RealE)
               +++ do
                    Ident "Or" <- lexP
                    v1 <- step readPrec
                    v2 <- step readPrec
                    return $ Less (v1 :: BoolE ) (v2 :: BoolE)
