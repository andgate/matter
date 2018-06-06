module Api.Modules where

import           Servant
import Servant.JS (vanillaJS, writeJSForAPI)


type ModulesAPI =
         "modules" :> Get '[JSON] [Entity Module]
    :<|> "modules" :> Capture "name" Text :> Get '[JSON] (Entity Module)
    :<|> "modules" :> ReqBody '[JSON] Module :> Post '[JSON] Int64

modulesApi :: Proxy ModulesAPI
modulesApi = Proxy

-- | The server that runs the UserAPI
modulesServer :: MonadIO m => ServerT ModulesAPI (AppT m)
modulesServer = allModules :<|> singleModule :<|> createModule

-- | Returns all users in the database.
allModules :: MonadIO m => AppT m [Entity Module]
allModules = do
    increment "allModules"
    logDebugNS "web" "allModules"
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleModule :: MonadIO m => Text -> AppT m (Entity Module)
singleModule str = do
    increment "singleModule"
    logDebugNS "web" "singleModule"
    maybeModule <- runDb (selectFirst [Md.ModuleName ==. str] [])
    case maybeModule of
         Nothing ->
            throwError err404
         Just m ->
            return m

-- | Creates a user in the database.
createModule :: MonadIO m => Module -> AppT m Int64
createModule m = do
    increment "createModule"
    logDebugNS "web" "creating a module"
    newModule <- runDb (insert (Module (moduleName m))
    return $ fromSqlKey newModule


-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
	writeJSForAPI (Proxy :: Proxy ModulesAPI) vanillaJS "./assets/api.js"