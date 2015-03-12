{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}
module PersistentData where

import Data.Text (Text)
import Database.Persist
import Database.Persist.TH
import Data.Time
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
PFItem
   nominal      Int
   contractType Text
   startDate    Day
   contractSpec Text
   deriving Show Generic

DbQuotes
   underlying  Text
   date        Day
   value       Double
   QuoteEntry  underlying date
   deriving Show Generic

DbModelData
   underlying  Text
   date        Day
   value       Double
   MDEntry     underlying date
   deriving Show Generic
|]
