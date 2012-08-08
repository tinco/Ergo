{-# LANGUAGE RecordWildCards #-}
module Ergo.AI where
import Ergo.Board
import Ergo.Game
import System.Random.Mersenne.Pure64
import Data.List
import Ergo.Utils
import Data.Array.Unboxed

type Move = (Event,Float)
type Strategy = Game -> [Move]

takeTurn :: Game -> Game
takeTurn game = maybe game id $ executeTurn game (fst move)
  where
    move = best . concatMap ($game) $ strategies

best :: [Move] -> Move
best moves = head $ sortBy (\a b -> compare (snd a) (snd b)) moves

strategies :: [Strategy]
strategies = [validMoveStrategy, threatStrategy]

validMoveStrategy :: Strategy
validMoveStrategy game = [randomMove game moves]
  where
    moves = map (\m -> (Selection m,0)) $ validMoves game

validMoves game = filter (validMove game) $ allPositions game

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

threatStrategy :: Strategy
threatStrategy game = map (\m->(Selection m,deltaThreat game m)) (validMoves game)

deltaThreat :: Game -> Position -> Float
deltaThreat game@Game{..} move = totalThreat game' - totalThreat game
  where
    game' = game {board = setPosition board player move}

totalThreat :: Game -> Float
totalThreat game@Game{..} = sum.concat.threatMap $ game

threatMap :: Game -> [[Float]]
threatMap game@Game{..} = foldl applyThreat m positions
  where
    positions = getPositions board
    size = length board
    m :: [[Float]]
    m = take size (repeat (take size $ repeat 0))
    ring1 = [
        (1,0),(-1,0),(0,-1),(0,1)
      ]
    ring2 = [
        (2,0),(-2,0),(0,-2),(0,2),
        (1,1),(-1,1),(1,-1),(-1,-1)
      ]
    ring0Value = 1
    ring1Value = 0.67
    ring2Value = 0.33
    noneModifier = 0.3
    applyThreat :: [[Float]] -> (Position, PositionState) -> [[Float]]
    applyThreat m (p,s) = m'''
      where
        polarity | s == player = -1
                 | otherwise = 1
        modifier | s == None = noneModifier * polarity
                 | otherwise = 1 * polarity
        v0 = ring0Value * modifier
        v1 = ring1Value * modifier
        v2 = ring2Value * modifier

        m' = addOnMap m p v0
        m'' = foldl (f v1 p) m' ring1
        m''' = foldl (f v2 p) m'' ring2

        f v (x,y) m (dx,dy) = addOnMap m (x + dx, y+dy) v

    addOnMap :: [[Float]] -> Position -> Float -> [[Float]]
    addOnMap m (x,y) v | (x > (length m) -1 || x < 0) ||
                         (y > (length m) -1 || y < 0) = m
                       | otherwise = update m x (update (m !! x) y v')
      where
        v' = ((m !! x) !! y) + v
