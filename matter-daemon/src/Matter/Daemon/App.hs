{-#LANGUAGE OverloadedStrings #-}

module Matter.Daemon.App where

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Sqlite

import Matter.Service.Lib (runApp)
import Matter.Service.Config (Config (..))
import Matter.Service.Models (migrateAll)

run :: IO ()
run =
 runStderrLoggingT $ withSqlitePool "matter.sqlite" 10 $ \pool -> liftIO $ do
		runResourceT $ flip runSqlPool pool $ 
			runMigration migrateAll :: IO ()

		runApp (Config pool)