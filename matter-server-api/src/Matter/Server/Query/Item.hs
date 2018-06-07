module Matter.Server.Query.Item where


import Database.Selda
import Database.Selda.Generic

import Matter.Server.Model.Item

getItems :: MonadSelda m => m [Item]
getItems = do
	its  <- query (select (gen items))
	return $ fromRels its


addItem :: MonadSelda m => Item -> m ()
addItem i = insertGen_ items [i] 