{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Matter.Service.Config (
  Config(..), App, AppServer, convertApp, runDb
) where


import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Trans.Class

import Database.Persist.Sql (SqlPersistT, runSqlPool, ConnectionPool)

import Servant

data Config = Config
  { cfgPool :: ConnectionPool
  }

newtype App a = App { runApp :: ReaderT Config (Handler) a}
  deriving(Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadError ServantErr)

type AppServer api = ServerT api App

convertApp :: Config -> App a -> Handler a
convertApp cfg (App m) = runReaderT m cfg

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks cfgPool
  liftIO $ runSqlPool query pool