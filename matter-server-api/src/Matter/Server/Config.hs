{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Matter.Server.Config where


import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Trans.Class
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Generic

import Servant

data Config = Config
  { cfgConn :: SeldaConnection
  }

-- newtype Handler a = Handler { runHandler' :: ExceptT ServantErr IO a }

deriving instance MonadMask Handler

newtype App a = App { runApp :: ReaderT Config (Handler) a}
  deriving(Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadError ServantErr)

type AppServer api = ServerT api App

convertApp :: Config -> App a -> Handler a
convertApp cfg (App m) = runReaderT m cfg


onDb :: SeldaM a -> App a
onDb m = do
  conn <- cfgConn <$> ask
  liftIO $ runSeldaT m conn