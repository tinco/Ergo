{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import System.Random
import Data.Word
import Control.Monad.Trans
import Control.Concurrent.MVar
import Data.Monoid (mconcat)
import qualified Data.Map as Map

import Ergo.Game
import Ergo.JSON

main = do
    games <- newMVar Map.empty
    scotty 5000 $ webGame games

webGame games = do
  let updateGame id game = updateMVar games $ Map.insert id game

  get "/" $ file "index.html"

  get "/game/new" $ do
    let game = newGame 6.5 19
    id <- newId
    updateGame id game 
    json $ (id,game)

  get "/game/:id" $ do
    id <- param "id"
    games' <- liftIO $ readMVar games
    case Map.lookup id games' of
      Just game -> json $ (id, game)
      Nothing -> json $ (404 ::Int, "Not found" :: String)

  post "/game/:id/move" $ do
    x <- param "x"
    y <- param "y"
    id <- param "id"
    games' <- liftIO $ readMVar games
    case Map.lookup id games' of
      Just game ->
        case executeTurn game (Selection (x,y)) of
          Just game' -> do
            updateGame id game'
            json $ (id, game')
          Nothing -> json $ (403 ::Int, "Not allowed" :: String)
      Nothing -> json $ (404 ::Int, "Not found: " ++ show id :: String)

updateMVar :: MVar a -> (a -> a) -> ActionM ()
updateMVar mvar f = liftIO $ do
    m <- takeMVar mvar
    putMVar mvar $ f m

newId :: ActionM([Char])
newId = liftIO $ do
    g <- newStdGen
    return $ map (\i -> (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])  !! i) $ take 16 $ (randomRs (0, 62) g)
