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
    (Char 'w') -> gstate {player = updatePlayerPosition (playerAccelerate (player gstate))} 
    (Char 'a') -> gstate {player = (player gstate){playerDirection = steerLeft (player gstate)}} 
    (Char 'd') -> gstate {player = (player gstate){playerDirection = steerRight (player gstate)}}
    _ -> gstate --other characters do not trigger movement

--Add a key to the set of pressed keys when a key is pressed. Removes when key is released.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate {keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate   = gstate {keys = S.delete k (keys gstate)}
handleInput _ gstate                     = gstate

--Go through the list of pressed keys and update the gamestate accordingly
update :: GameState -> GameState
update gstate | keys gstate == S.empty = baseState
              | otherwise              = foldr f e (keys gstate)
  where
    f k acc       | S.member k set && k == Char 'w' = inputKey k baseState
                  | S.member k set = inputKey k acc
                  | otherwise      = baseState
    e             = gstate 
    set           = S.fromList [Char 'w', Char 'a', Char 'd']
    baseState     = gstate {player = updatePlayerPosition (playerSlowDown (player gstate))}

--Update the player position

playerSlowDown :: Player -> Player
playerSlowDown  p@(Player {playerSpeed = v}) = p{playerSpeed = newSpeed}
  where
    newSpeed | v > 0     = v - 0.1
             | otherwise = 0

playerAccelerate :: Player -> Player
playerAccelerate p@(Player {playerSpeed = v}) = p{playerSpeed = newSpeed}
  where 
    newSpeed       | v < maxSpeed = v + acceleration p
                   | otherwise = v
    maxSpeed       = 10 

updatePlayerPosition :: Player -> Player
updatePlayerPosition p@(Player {playerDirection = Angle a, playerSpeed = v, playerPosition = Point x y}) = p{playerPosition = newPosition} 
  where 
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180 --convert from degrees to radials
    newPosition = Point (xComponent * v + x) (yComponent * v + y)

--Increase and decrease player angle, bounds at 0 and 360
steerLeft :: Player -> Angle
steerLeft p@(Player {playerDirection = Angle a}) | a + 5 > 360 = Angle (a + 5 - 360)
                                                 | otherwise    = Angle (a + 5)

steerRight :: Player -> Angle
steerRight p@(Player {playerDirection = Angle a}) | a - 5 < 0 = Angle(a - 5 + 360)
                                                  | otherwise  = Angle (a - 5)
