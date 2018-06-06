{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

-- This Template Haskell just generates a 'Comment' datatype and some helper
-- functions for working with it.  For more information, check out Persistent's
-- <http://www.yesodweb.com/book/persistent documentation>.

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Module json
  name Text
  UniqueName name
  deriving Eq Read Show Typeable

ModuleIndice json
  parent ModuleId
  child ModuleId
  deriving Eq Read Show Typeable

Item json
  content Text
  deriving Eq Read Show Typeable

ModuleItem json
  parent ModuleId
  child ItemId
|]


doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadIO m) => SqlPersistT IO b -> ConnectionPool -> m b
runDb query pool =
  liftIO $ runSqlPool query pool