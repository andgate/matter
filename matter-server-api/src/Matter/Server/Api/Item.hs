{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Matter.Server.Api.Item where

import Servant

import Data.Text
import Matter.Server.Config
import Matter.Server.Model.Item
import qualified Matter.Server.Query.Item as Q



type ItemAPI = GetItems
          :<|> AddItem


itemAPI :: Proxy ItemAPI
itemAPI = Proxy

itemServer :: AppServer ItemAPI
itemServer = getItems :<|> addItem


type GetItems = Get '[JSON] [Item]

getItems :: App [Item]
getItems = onDb Q.getItems


type AddItem = ReqBody '[JSON] Item :> Post '[JSON] ()

addItem :: Item -> App ()
addItem = onDb . Q.addItem
