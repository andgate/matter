{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Matter.Server.Model.Item where

import Data.Aeson
import Data.Text (Text, unpack)
import Database.Selda
import Database.Selda.Generic
import GHC.Generics


data Item = Item
  { itemContents :: Text
  } deriving Generic

instance ToJSON Item
instance FromJSON Item

items :: GenTable Item
items = genTable "items" [itemContents :- primaryGen]