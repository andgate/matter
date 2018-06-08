module Matter.Service.Query.Module where

import Database.Persist.Class
import Database.Persist.Sql

import Matter.Service.Models

getModules :: SqlPersistT IO [Module]
getModules = map entityVal <$> selectList [] [Desc ModuleName]


addModule :: Module -> SqlPersistT IO  ()
addModule m = insert_ m