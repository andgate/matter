{-# LANGUAGE RankNTypes #-}
module Config where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks)
import Control.Monad.Trans.Control (MonadBaseControl(StM, liftBaseWith, restoreM))
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool)
import Network.Wai.Handler.Warp (Port)
import Network.Wai (Application)
import System.ReadEnvVar (lookupEnvDef, readEnvDef)

-------------------
-- Configuration --
-------------------

-- | This 'Config' object is used to store environment about our application.
-- It is created on startup and passed to all the handlers.
data Config = Config
  { configPool :: !ConnectionPool  -- ^ A pool of database connections.
  , configPort :: !Port            -- ^ 'Port' to listen on.
  }

-- | Number of simultaneous database connections to use in the
-- 'ConnectionPool'.
type DbPoolConnNum = Int

-- | Create a 'ConnectionPool' for database connections based on a
-- 'ConnectionString'.
makePoolFromUrl
  :: DbPoolConnNum      -- ^ Number of database connections to use.
  -> ConnectionString
  -> IO ConnectionPool
makePoolFromUrl dbConnNum connectionString =
  runStdoutLoggingT $ createPostgresqlPool connectionString dbConnNum


-- | Create a 'Config' based on environment variables, using defaults if the
-- environment variables don't exist.
createConfigFromEnvVars :: IO Config
createConfigFromEnvVars = do
  port <- readEnvVarDef "PORT" 8080
  dbConnNum <- readEnvVarDef "DATABASE_CONNECTION_NUM" 10
  dbConnectionString <-
    lookupEnvDef
      "DATABASE_URL"
      "postgres://mydbuser:mydbpass@localhost:5432/mydb"
  pool <- makePoolFromUrl dbConnNum dbConnectionString
  pure Config {configPool = pool, configPort = port}

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServantErr m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadBase IO
             , MonadError ServantErr
             , MonadIO
             , MonadReader Config
             )

type App = AppT IO

instance MonadBaseControl IO App where
  type StM App a = Either ServantErr a

  liftBaseWith
    :: forall a.
       ((forall x. App x -> IO (Either ServantErr x)) -> IO a) -> App a
  liftBaseWith f =
    AppT $
    ReaderT $ \r ->
      ExceptT $
      fmap Right $
      f $ \(AppT readerTExceptT) -> runExceptT $ runReaderT readerTExceptT r

  restoreM :: forall a. Either ServantErr a -> App a


runDb :: ReaderT SqlBackend MyApiM a -> MyApiM a
runDb query = do
  pool <- reader configPool
  runSqlPool query pool
  