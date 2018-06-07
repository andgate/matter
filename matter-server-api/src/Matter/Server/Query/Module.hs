module Matter.Server.Query.Module where


import Database.Selda
import Database.Selda.Generic

import Matter.Server.Model.Module

getModules :: MonadSelda m => m [Module]
getModules = do
	ms  <- query (select (gen modules))
	return $ fromRels ms


addModule :: MonadSelda m => Module -> m ()
addModule m = insertGen_ modules [m] 