module Ergo.Board where
import Ergo.Utils
import Data.List ((\\), nub)

-- The Go board
data Player = Black | White | None deriving (Eq, Show)
type PositionState = Player

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black

type Position = (Int, Int)
type Board = [[PositionState]]

-- Makes a new board of the given size.
newBoard :: Int -> Board  
newBoard size = take size (repeat (take size $ repeat None))

-- Returns the state of a position
getPosition :: Board -> Position -> PositionState
getPosition board (x, y)
  = ((board !! x) !! y)

-- Sets a position on the board.
setPosition :: Board -> PositionState -> Position -> Board
setPosition board state (x, y)
  = update board x (update (board !! x) y state)
    
-- Sets a position and then resolves any captures that may result to that move
-- and returns the points awarded for those captures.
move :: Board -> PositionState -> Position -> (Board, Int)
move board color position
  = (board'', score)
  where
    board' = setPosition board color position
    enemyNeigbours = [b | b <- neighbours board' position, getPosition board' b == nextPlayer color]
    deadNeighbours = filter (isGroupDead board') (map (group board') enemyNeigbours)
    score = foldl (\t g -> t+(length g)) 0 deadNeighbours
    board'' = foldl killGroup board' deadNeighbours

-- Returns the neighbouring positions to a position
neighbours :: Board -> Position -> [Position]
neighbours board (x,y)
  = neighbourPositions
  where
    size   = length board
    left   | x > 0 = [(x-1, y)]
           | otherwise = []
    right  | x < (size - 1) = [(x+1, y)]
           | otherwise = []
    top    | y < (size - 1) = [(x,y+1)]
           | otherwise = []
    bottom | y > 0 = [(x, y-1)]
           | otherwise = []
    neighbourPositions = left ++ right ++ top ++ bottom

 
-- Returns the liberties a single position has.
posLiberties :: Board -> Position -> [Position]
posLiberties board position
  = [ p | p <- (neighbours board position), getPosition board p == None]

-- Returns the group of stones a position is a part of.    
group :: Board -> Position -> [Position]
group board position
  = group' board [] [position]

group' :: Board -> [Position] -> [Position] -> [Position]
group' board collected fringe
  | fringe' /= [] = group' board newCollected fringe'
  | otherwise = newCollected
  where
    color = getPosition board (head fringe)
    newCollected = collected ++ fringe
    fringe' = (nub 
                  ( filter
                    (\x -> getPosition board x == color)  
                    (concat (map (neighbours board) fringe))
                  )
                 ) \\ collected

-- Returns the liberties a group of stones has
groupLiberties :: Board -> [Position] -> [Position]
groupLiberties board group
  = nub (concat (map (posLiberties board) group))

-- Returns wether a group has any liberties.
isGroupDead :: Board -> [Position] -> Bool
isGroupDead board group
  = groupLiberties board group == []
  
-- Removes a group from the board.
killGroup :: Board -> [Position] -> Board
killGroup board group
  = foldl (\b p -> setPosition b None p) board group

-- Example group used for testing      
exGroup = [[Black, Black, Black] ,
           [Black, None,  Black] ,
           [Black, Black, None ] ]

-- Returns the positions of the stones on the board. 
getPositions :: Board -> [(Position,Player)]      
getPositions board
  = concat (zipWith (\ row space_x ->
                 (zipWith (\ stone space_y ->
                   ((space_x, space_y), stone)
                  ) row [0..boardSize -1] ) -- Mirror in y-axis
                ) board [0..boardSize -1] )
  where
    boardSize = length board
      
-- Gets all stones with their positions
getStones :: Board -> [(Position, Player)]
getStones board
  = filter (\(_,c)-> c /= None) (getPositions board)

-- Gets all groups of stones      
getGroups :: Board -> [([Position], Player)]
getGroups board
  = groups' board ([p | (p,c)<-(getStones board)]) []
    
-- Gets all territories (linked empty positions)
getTerritories :: Board -> [[Position]]
getTerritories board
  = [g | (g,k) <- groups' board [p | (p,c)<- getPositions board, c == None] []]
    
-- Returns the owner of a territory or None if it is contested.
getOwner :: Board -> [Position] -> Player
getOwner board territory
  | borderColours == [Black] = Black
  | borderColours == [White] = White
  | otherwise = None
  where
    borderColours = ( nub 
                      ( filter
                        (\ x-> x /= None)  
                        (map (getPosition board) (concat (map (neighbours board) territory)))
                      )
                     )

-- Helper method to getGroups and getTerritories    
groups' board [] groups = groups
groups' board ((position):positions) groups
  = groups' board newPositions newGroups
  where
    newGroup = (group board position)
    color = getPosition board position
    newGroups = (newGroup, color) : groups
    newPositions = positions \\ newGroup
