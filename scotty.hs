{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Data.Aeson.TH(deriveJSON)
import Data.Aeson
import Web.Scotty

import Data.Monoid (mconcat)

import Ergo.Game
import Ergo.Board

$(deriveJSON Prelude.id ''Game)
instance ToJSON Player where
  toJSON Black = "Black"
  toJSON White = "White"
  toJSON None = "None"

instance FromJSON Player where
  parseJSON (String "Black") = return Black
  parseJSON (String "White") = return White
  parseJSON (String "None") = return None

main = scotty 5000 $ do
  get "/word/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  get "/json" $ do
    Web.Scotty.json $ newGame 6.5 19

