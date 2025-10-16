module Asteroid where

import Model
import System.Random

updateAsteroids :: GameState -> GameState
updateAsteroids gstate@(GameState{asteroids = asteroids }) | null asteroids = moveForward gstate{asteroids = generateRandomAsteroidList (rg gstate) 10}
                                                           | otherwise = moveForward gstate 

moveForward :: GameState -> GameState
moveForward gstate = gstate {asteroids = map updateAsteroidPosition (asteroids gstate)}

updateAsteroidPosition :: Asteroid -> Asteroid
updateAsteroidPosition ast@(Asteroid {asteroidSpeed = v, asteroidDirection = Angle a, asteroidPosition = Point x y}) = ast{asteroidPosition = newPosition}  
    where 
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180 --convert from degrees to radials
    newPosition = Point (xComponent * v + x) (yComponent * v + y)

generateRandomAsteroid :: StdGen -> (Asteroid, StdGen)
generateRandomAsteroid g = 
  let 
    (v, gen1) = randomR (3, 6) g      
    (a, gen2) = randomR (0, 10) gen1
    (x, gen3) = randomR (0, 10) gen2
    (y, gen4) = randomR (0, 10) gen3
    (s, gen5) = randomR (10, 30) gen4
  in
    ((Asteroid {asteroidSpeed = v, asteroidDirection = Angle (a * 36), asteroidPosition = Point (x*40) (y*40), asteroidSize = s}), gen5)

generateRandomAsteroidList :: StdGen -> Int -> [Asteroid]
generateRandomAsteroidList _ 0 = []
generateRandomAsteroidList g n = let (asteroid, gen1) = generateRandomAsteroid g
                                 in asteroid : generateRandomAsteroidList gen1 (n-1)