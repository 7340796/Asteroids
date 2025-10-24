module Animations where
import Model

shrinkPlayer :: Float -> GameState -> GameState
shrinkPlayer seconds gstate = gstate{player = newPlayer}
    where
        newPlayer = (player gstate){playerSize = playerSize (player gstate) - seconds}