module Animations where
import Model
import Data.Fixed

shrinkPlayer :: Float -> GameState -> GameState
shrinkPlayer seconds gstate = gstate{player = newPlayer}
    where
        curPlayer = player gstate
        newPlayer = (player gstate){playerSize = newSize, animationTimer = newAnimationTimer}
        newAnimationTimer | animationTimer curPlayer == 0 = seconds --When the timer = 0, it has not started yet and gets initiated with the curernt elapsed game time
                          | otherwise = animationTimer curPlayer
        time = seconds - animationTimer (player gstate)            --How long the animation is playing
        newSize | time /= 0 && (playerSize curPlayer >= 0)= playerSize curPlayer -1
                | otherwise = playerSize curPlayer