module Matter.Service.Query.Item where


import Database.Persist.Class
import Database.Persist.Sql

import Matter.Service.Models

getItems :: SqlPersistT IO [Item]
getItems = map entityVal <$> selectList [] [Desc ItemContents]


addItem :: Item -> SqlPersistT IO ()
addItem i = insert_ i 