{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE CPP             #-}

module Matter.Server.Lib ( startApp ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import Database.Selda hiding (app)
import Database.Selda.Backend
import Database.Selda.Generic
import Database.Selda.SQLite

import Matter.Server.Config
import Matter.Server.Api.Module
import Matter.Server.Api.Item
import Matter.Server.Model.Module
import Matter.Server.Model.Item

type API = "modules" :> ModuleAPI
      :<|> "items"   :> ItemAPI

startApp :: IO ()
startApp = withSQLite "matter.sqlite" $ do
  -- Make sure tables exist
  tryCreateTable (gen modules)
  tryCreateTable (gen items)

  -- Generate config
  cfg <- Config <$> seldaConnection

  liftIO $ Warp.run 8080 (app cfg)

app :: Config -> Wai.Application
app cfg = serve api (server cfg)

api :: Proxy API
api = Proxy

server :: Config -> Server API
server cfg = hoistServer api (convertApp cfg) appServer

appServer :: AppServer API
appServer = moduleServer
       :<|> itemServer