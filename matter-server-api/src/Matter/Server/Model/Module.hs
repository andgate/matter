{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Matter.Server.Model.Module where

import Data.Aeson
import Data.Text (Text, unpack)
import Database.Selda
import Database.Selda.Generic
import GHC.Generics hiding (moduleName)


data Module = Module
  { moduleName :: Text
  } deriving Generic

instance ToJSON Module
instance FromJSON Module

modules :: GenTable Module
modules = genTable "modules" [moduleName :- primaryGen]