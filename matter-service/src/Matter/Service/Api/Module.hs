{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Matter.Service.Api.Module where

import Servant
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (listToMaybe)

import Matter.Service.Config
import Matter.Service.Models
import qualified Matter.Service.Query.Module as Q


type ModuleAPI = GetModules
            :<|> AddModule

moduleAPI :: Proxy ModuleAPI
moduleAPI = Proxy


moduleServer :: AppServer ModuleAPI
moduleServer = getModules
          :<|> addModule

type GetModules = Get '[JSON] [Module]

getModules :: App [Module]
getModules = runDb Q.getModules


type AddModule = ReqBody '[JSON] Module :> Post '[JSON] ()

addModule :: Module -> App ()
addModule = runDb . Q.addModule