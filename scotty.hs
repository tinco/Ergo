{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)

import Ergo.Game
import Ergo.JSON

main = scotty 5000 webGame

webGame = do
  get "/" $ file "index.html"
  get "/game/new" $ do
    json $ newGame 6.5 19

