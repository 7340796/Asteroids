-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game 
import System.Random
import qualified Data.Set as S -- import datatype Set
import Player

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = do 
    return (updatePlayer gstate)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

--Add a key to the set of pressed keys when a key is pressed. Removes when key is released.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate {keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate   = gstate {keys = S.delete k (keys gstate)}
handleInput _ gstate                     = gstate


