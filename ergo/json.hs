{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Ergo.JSON where
import Data.Aeson.TH(deriveJSON)
import Data.Aeson
import Ergo.Board
import Ergo.Game

$(deriveJSON Prelude.id ''Game)
instance ToJSON Player where
  toJSON Black = "Black"
  toJSON White = "White"
  toJSON None = "None"

instance FromJSON Player where
  parseJSON (String "Black") = return Black
  parseJSON (String "White") = return White
  parseJSON (String "None") = return None
