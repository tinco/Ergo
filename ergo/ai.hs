{-# LANGUAGE RecordWildCards #-}
module Ergo.AI where
import Ergo.Board
import Ergo.Game

import Data.List

type Move = (Event,Float)
type Strategy = Game -> [Move]

takeTurn :: Game -> Game
takeTurn game = maybe game id $ executeTurn game (fst move)
  where
    move = best . concatMap ($game) $ strategies

best :: [Move] -> Move
best moves = head $ sortBy (\a b -> compare (snd a) (snd b)) moves

strategies :: [Strategy]
strategies = [validMoveStrategy]

validMoveStrategy :: Strategy
validMoveStrategy game = map (\m -> (Selection m,0)) $ filter (validMove game) $ allPositions game

allPositions :: Game -> [Position]
allPositions game@Game{..} = map fst $ getPositions board
