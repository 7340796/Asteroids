-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do --randomNumber <- randomIO
       --let newNumber = abs randomNumber `mod` 10
       let newNumber = playerSize (player gstate) + 20
       return $ gstate { player = Player (Point 0 0) (Angle 0) 0 5 newNumber, elapsedTime = 0}
  | otherwise
  = -- Just update the elapsed time
    do 
      let oldnumber = playerSize (player gstate)
      return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { score = Score 7}
inputKey _ gstate = gstate -- Otherwise keep the same