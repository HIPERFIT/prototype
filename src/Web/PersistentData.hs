{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}
module PersistentData where

import Data.Text (Text)
import Database.Persist
import Database.Persist.TH
import Data.Time
import GHC.Generics

defaultPortfolioId :: Integer
defaultPortfolioId = 1

defaultUserId :: Integer
defaultUserId = 1

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Portfolio
   label String
   owner UserId
   PortfLabel label
   deriving Show Generic     

PFItem
   nominal      Int
   contractType Text
   startDate    Day
   contractSpec Text
   portfolioId  PortfolioId
   deriving Show Generic

DbQuotes
   underlying  Text
   date        Day
   value       Double
   userId      UserId
   QuoteEntry  underlying date
   deriving Show Generic

DbModelData
   underlying  Text
   date        Day
   value       Double
   userId      UserId
   MDEntry     underlying date
   deriving Show Generic

User
  username  String
  password  String
  Username username
|]
