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
  = do 
    return (update gstate)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

--Maps a key to an action
inputKey :: Key -> GameState -> GameState
inputKey k gstate
  = case k of 
    (Char 'w') -> gstate {player = (player gstate){playerPosition = updatePlayerPosition (player gstate)}} 
    (Char 'a') -> gstate {player = (player gstate){playerDirection = steerLeft (player gstate)}} 
    (Char 'd') -> gstate {player = (player gstate){playerDirection = steerRight (player gstate)}}
    _ -> gstate --other characters do not trigger movement

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

--Update the player position
updatePlayerPosition :: Player -> Model.Point
updatePlayerPosition p@(Player {playerDirection = Angle a, playerSpeed = v, playerPosition = Point x y}) = Point (xComponent * v + x) (yComponent * v + y) 
  where
    xComponent = cos (convert a)
    yComponent = sin (convert a)
    convert a  = a * pi / 180 --convert from degrees to radials

--Increase and decrease player angle, bounds at 0 and 360
steerLeft :: Player -> Angle
steerLeft p@(Player {playerDirection = Angle a}) | a + 10 > 360 = Angle (a + 20 - 360)
                                                 | otherwise    = Angle (a + 20)

steerRight :: Player -> Angle
steerRight p@(Player {playerDirection = Angle a}) | a - 10 < 0 = Angle(a - 20 + 360)
                                                  | otherwise  = Angle (a - 20)
