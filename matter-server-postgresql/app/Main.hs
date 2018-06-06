module Main where


import Config
import App

defaultMain :: IO ()
defaultMain = do
  config <- createConfigFromEnvVars
  runSqlPool (runMigration migrateAll) $ configPool config
  putStrLn $
    "running servant-on-heroku on port " <> show (configPort config) <> "..."
  run (configPort config) . logStdoutDev $ app config


main :: IO ()
main = defaultMain