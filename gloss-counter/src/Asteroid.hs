module Asteroid where

import Model
import System.Random

updateAsteroids :: GameState -> GameState
updateAsteroids gstate = moveForward gstate

moveForward :: GameState -> GameState
moveForward gstate = gstate {asteroids = map updatePosition (asteroids gstate)}

updatePosition :: Asteroid -> Asteroid
updatePosition ast@(Asteroid {asteroidSpeed = v, asteroidDirection = Angle a, asteroidPosition = Point x y}) = ast{asteroidPosition = newPosition}  
    where 
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180 --convert from degrees to radials
    newPosition = Point (xComponent * v + x) (yComponent * v + y)

