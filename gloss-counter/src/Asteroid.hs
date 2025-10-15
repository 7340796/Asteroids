module Asteroid where

import Model
import System.Random

updateAsteroids :: GameState -> GameState
updateAsteroids gstate@(GameState{asteroids = asteroids }) | null asteroids = moveForward gstate{asteroids = [generateRandomAsteroid (rg gstate)]}
                                                           | otherwise = moveForward gstate 

moveForward :: GameState -> GameState
moveForward gstate = gstate {asteroids = map updatePosition (asteroids gstate)}

updatePosition :: Asteroid -> Asteroid
updatePosition ast@(Asteroid {asteroidSpeed = v, asteroidDirection = Angle a, asteroidPosition = Point x y}) = ast{asteroidPosition = newPosition}  
    where 
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180 --convert from degrees to radials
    newPosition = Point (xComponent * v + x) (yComponent * v + y)

generateRandomAsteroid :: StdGen -> Asteroid
generateRandomAsteroid g = 
  let 
    (v, gen1) = randomR (3, 8) g      
    (a, gen2) = randomR (0, 10) gen1
    (x, gen3) = randomR (0, 100) gen2
    (y, gen4) = randomR (0, 100) gen3
    (s, gen5) = randomR (10, 30) gen4
  in
    (Asteroid {asteroidSpeed = v, asteroidDirection = Angle (a * 36), asteroidPosition = Point (x*4) (y*4), asteroidSize = s})

