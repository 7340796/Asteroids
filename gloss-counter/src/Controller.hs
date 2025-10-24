-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as S -- import datatype Set
import Player
import Asteroid
import Bullet
import Enemy
import Collisions
import Highscore
import System.IO
import Prelude

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = do
    let
      newState = stateChange gstate{elapsedTime = elapsedTime gstate + secs}
    case state newState of 
      Playing  -> return $ checkForCollisions ((updateBullets.updateAsteroids.updateEnemies.updatePlayer) newState)
      GameOver -> putHighScore newState

putHighScore :: GameState -> IO GameState
putHighScore gstate = do
  writeHighscore (score gstate)
  return gstate

stateChange :: GameState -> GameState
stateChange gstate | lives gstate <= 0 = gstate{state = GameOver}
                   | otherwise = gstate

--Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

--Add a key to the set of pressed keys when a key is pressed. Removes when key is released.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate {keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate   = gstate {keys = S.delete k (keys gstate)}
handleInput _ gstate                     = gstate

