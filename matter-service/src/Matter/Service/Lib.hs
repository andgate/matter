{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Matter.Service.Lib ( runApp ) where


import Database.Persist.Sql
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import Matter.Service.Config
import Matter.Service.Api.Module
import Matter.Service.Api.Item
import Matter.Service.Models

type API = "modules" :> ModuleAPI
      :<|> "items"   :> ItemAPI


runApp :: Config -> IO ()
runApp cfg = Warp.run 8080 (app cfg)

app :: Config -> Wai.Application
app cfg = serve api (server cfg)

api :: Proxy API
api = Proxy

server :: Config -> Server API
server cfg = hoistServer api (convertApp cfg) appServer

appServer :: AppServer API
appServer = moduleServer
       :<|> itemServer