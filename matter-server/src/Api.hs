{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text

import Database.Persist

import Models

import Servant.API



type Api =
       "modules" :> ReqBody '[JSON] Module :> Post '[JSON] (Maybe (Key Module))
  :<|> "modules" :> Capture "name" Text  :> Get  '[JSON] (Maybe Module)

api :: Proxy Api
api = Proxy