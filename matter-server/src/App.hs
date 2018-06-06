{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App 
	( module App
	, Application
	)
	where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist (Entity(entityVal), (==.))
import           Database.Persist.Class (insert, selectFirst)
import           Database.Persist.Sql ( ConnectionPool, SqlBackend, runSqlPool, runSqlPersistMPool, runMigration)
import           Data.Text (Text)
import           Network.Wai.Handler.Warp as Warp

import           Servant

import           Api
import           Models



app :: ConnectionPool -> Application
app pool = serve api $ server pool
{-
app :: Config -> Application
app config = serve (Proxy :: Proxy Api) apiServer
  where
    apiServer :: Server Api
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: App :~> Handler
    naturalTrans = NT transformation

    -- This represents a natural transformation from 'App' to 'Handler'.
    -- This consists of unwrapping the 'App', running the
    -- @'ReaderT' 'Config'@, and wrapping the resulting value back up in a
    -- 'Handler'.
    transformation :: forall a . App a -> Handler a
    transformation = Handler . flip runReaderT config . runApp
-}

server :: ConnectionPool -> Server Api
server pool =
  moduleAddH :<|> moduleGetH
  where
    moduleAddH newModule = liftIO $ moduleAdd newModule
    moduleGetH name    = liftIO $ moduleGet name

    moduleAdd :: Module -> IO (Maybe (Key Module))
    moduleAdd newModule = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [ModuleName ==. (moduleName newModule)] []
      case exists of
        Nothing -> Just <$> insert newModule
        Just _ -> return Nothing

    moduleGet :: Text -> IO (Maybe Module)
    moduleGet name = flip runSqlPersistMPool pool $ do
      mModule <- selectFirst [ModuleName ==. name] []
      return $ entityVal <$> mModule