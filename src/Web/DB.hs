{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}

module DB where

import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger

runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT $ runResourceT . withSqliteConn "proto.sqlite3" . runSqlConn $ query

getByKey intId = runDb $ get $ toSqlKey (fromIntegral intId)

