{-#LANGUAGE OverloadedStrings #-}

module Matter.Server.App where

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Postgresql

import Matter.Service.Lib (runApp)
import Matter.Service.Config (Config (..))
import Matter.Service.Models (migrateAll)


connStr = "host=localhost dbname=test user=test password=test port=5432"

run :: IO ()
run = do
  runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll

    runApp (Config pool)