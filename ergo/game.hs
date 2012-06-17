module Ergo.Game where
import Ergo.Board
import Ergo.Utils

-- Main Flow:
-- Turn * -> (make move) -> Turn **
-- Turn * -> (make invalid move) **
-- Turn * -> (pass (1st pass)) -> Turn **
-- Turn * -> (pass (2nd pass)) -> Score
-- Turn * -> (resign) -> End(**)
-- Score -> (mark dead groups (groups left)) -> Score
-- Score -> (mark dead groups (no groups left)) -> End(*)

-- Rules: http://en.wikipedia.org/wiki/Rules_of_Go
-- Extra rules:
-- Suicide is forbidden

type Score = (Float, Float)
data Phase = Turn | Score | End

data Game = Game {
    board :: Board,
    history :: [Board],
    player :: Player,
    score :: Score,
    winner :: Player,
    passes :: Int,
    liberties :: Bool
}

data Event = Selection Position | Pass | Resign

type GameState = (Phase, Game)

newGame :: Float -> Int -> Game
newGame komi size = Game {
    board = newBoard size,
    history = [],
    player = Black,
    score = (0, komi),
    winner = None,
    passes = 0,
    liberties = False
}

gameFSA :: GameState -> Event -> GameState
gameFSA (Turn, game) event =
    case executeTurn game event of
        Just newGame | (passes newGame) == 2 -> (Score, newGame)
                     | (winner newGame) /= None -> (End, newGame)
                     | otherwise -> (Turn, newGame)
        Nothing -> (Turn, game)

gameFSA (Score, game) event
    | (winner newGame) /= None = (End, newGame)
    | otherwise = (Score, newGame)
  where
    newGame = executeScore game event

-- Turn
-- A stone may not be placed upon another stone.
-- A stone may not result in an own group dying.
-- A stone may not recreate the previous situation.
executeTurn :: Game -> Event -> Maybe Game
executeTurn game@Game{board=board, player=player, history=history, score=score} (Selection position)
  | not invalidpos && not suicide && not ko = Just newGame
  | otherwise = Nothing 
  where
    (s_b, s_w) = score
    invalidpos = getPosition board position /= None
    (newBoard, p_score) | not invalidpos = move board player position
                        | otherwise = (board, 0)
    newScore | player == Black = (s_b + (fromIntegral p_score), s_w)
             | otherwise = (s_b, s_w + (fromIntegral p_score))
                
    suicide = isGroupDead newBoard (group newBoard position)
    ko | length history > 2 = newBoard == (history !! (length history - 2))
       | otherwise = False
    --Score
    newHistory = history ++ [newBoard]
    newGame = game{board=newBoard, history=newHistory, score=newScore, passes=0}

executeTurn game Resign = Just (game {winner = nextPlayer (player game)})
executeTurn game Pass = Just (game {passes = passes game + 1})

executeScore = undefined
