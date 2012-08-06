{-# LANGUAGE RecordWildCards #-}
module Ergo.AI where
import Ergo.Board
import Ergo.Game
import System.Random.Mersenne.Pure64
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
validMoveStrategy game = [randomMove game moves]
  where
    moves = map (\m -> (Selection m,0)) $ filter (validMove game) $ allPositions game

allPositions :: Game -> [Position]
allPositions game@Game{..} = map fst $ getPositions board

randomMove game@Game{..} moves = moves !! (r `mod` (length moves))
  where
    g = pureMT (fromIntegral seed)
    (r, _) = randomInt g
    positions = getPositions board
    seed = sum $ map toSeed positions 
    toSeed ((i,j), Black) = (i * 3 + j * 5) * 6659
    toSeed ((i,j), White) = (i * 3 + j * 5) * 5233
    toSeed ((i,j), None) = (i * 3 + j * 5) * 4783
