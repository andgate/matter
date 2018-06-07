{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Matter.Server.Api.Module where

import Servant
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (listToMaybe)

import Matter.Server.Config
import Matter.Server.Model.Module
import qualified Matter.Server.Query.Module as Q


type ModuleAPI = GetModules
            :<|> AddModule

moduleAPI :: Proxy ModuleAPI
moduleAPI = Proxy


moduleServer :: AppServer ModuleAPI
moduleServer = getModules
          :<|> addModule

type GetModules = Get '[JSON] [Module]

getModules :: App [Module]
getModules = onDb Q.getModules


type AddModule = ReqBody '[JSON] Module :> Post '[JSON] ()

addModule :: Module -> App ()
addModule = onDb . Q.addModule