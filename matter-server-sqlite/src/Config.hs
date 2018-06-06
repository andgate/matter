module Config where

import Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                                         , runSqlPool, runSqlPersistMPool
                                         , runMigration, selectFirst, (==.)
                                         , insert, entityVal)
import           Data.String.Conversions (cs)
import qualified Network.Wai.Handler.Warp as Warp

import App
import Models


mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 8080 =<< mkApp sqliteFile