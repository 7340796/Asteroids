-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game 
import System.Random
import qualified Data.Set as S -- import datatype Set

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = return (update gstate)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

--Maps a key to an action
inputKey :: Key -> GameState -> GameState
inputKey k gstate
  = case k of 
    (Char 'w') -> gstate {player = moves (player gstate) North}
    (Char 'a') -> gstate {player = moves (player gstate) West}
    (Char 's') -> gstate {player = moves (player gstate) South}
    (Char 'd') -> gstate {player = moves (player gstate) East}
    _ -> gstate --other characters do not trigger movement

--Defines player movement in different directions
moves :: Player -> Direction -> Player
moves p@(Player {playerPosition = Point x y}) North = p{ playerPosition = Point x (y + 10)}
moves p@(Player {playerPosition = Point x y}) East  = p{ playerPosition = Point (x + 10) y}
moves p@(Player {playerPosition = Point x y}) South = p{ playerPosition = Point x (y - 10)}
moves p@(Player {playerPosition = Point x y}) West  = p{ playerPosition = Point (x -10) y}

--Add a key to the set of pressed keys when a key is pressed. Removes when key is released.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate { keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate = gstate {keys = S.delete k (keys gstate)}
handleInput _ gstate = gstate

--Go through the list of pressed keys and update the gamestate accordingly
update :: GameState -> GameState
update gstate = foldr f e (keys gstate)
  where 
    f     = inputKey
    e     = gstate 
