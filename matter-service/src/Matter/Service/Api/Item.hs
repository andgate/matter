{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Matter.Service.Api.Item where

import Servant

import Data.Text
import Matter.Service.Config
import Matter.Service.Models
import qualified Matter.Service.Query.Item as Q



type ItemAPI = GetItems
          :<|> AddItem


itemAPI :: Proxy ItemAPI
itemAPI = Proxy

itemServer :: AppServer ItemAPI
itemServer = getItems :<|> addItem


type GetItems = Get '[JSON] [Item]

getItems :: App [Item]
getItems = runDb Q.getItems


type AddItem = ReqBody '[JSON] Item :> Post '[JSON] ()

addItem :: Item -> App ()
addItem = runDb . Q.addItem
