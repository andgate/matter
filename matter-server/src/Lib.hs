{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


data Module = Module
  { moduleName :: String
  , moduleChildren :: [Module]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Module)

type API = "modules" :> Get '[JSON] [Module]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return modules

modules :: [Module]
modules = 
  [ Module "Math" []
  , Module "String" []
  , Module "Category"
      [ Module "Functor" []
      , Module "Applicative" []
      , Module "Monad" []
      ]
  ]
