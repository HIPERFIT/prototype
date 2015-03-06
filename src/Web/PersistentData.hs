{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving #-}
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
|]
