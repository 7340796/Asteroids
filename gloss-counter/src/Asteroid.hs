module Asteroid where

import Model
import System.Random
import GHC.Float (int2Float)

instance Entity Asteroid where
  updatePosition = updateAsteroidPosition
  getHitbox ast = HitBox (asteroidSize ast) (asteroidPosition ast)

--Spawn asteroids when there are none in the playing field
updateAsteroids :: GameState -> GameState
updateAsteroids gstate@(GameState{asteroids = asteroids }) | null asteroids = moveForward gstate{asteroids = generateRandomAsteroidList (rg gstate) 7}
                                                           | otherwise      = moveForward gstate

moveForward :: GameState -> GameState
moveForward gstate = gstate {asteroids = map (`updateAsteroidPosition` gstate) (asteroids gstate)}

updateAsteroidPosition :: Asteroid -> GameState -> Asteroid
updateAsteroidPosition ast@(Asteroid {asteroidSpeed = v, asteroidDirection = Angle a, asteroidPosition = Point x y}) gstate = ast{asteroidPosition =  Point (boundsPositionX newPosition) (boundsPositionY newPosition)}
    where
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180 --convert from degrees to radians
    newPosition = Point (xComponent * v + x) (yComponent * v + y)
    boundsPositionX newPosition@(Point x y) | x > maxX = -x
                                            | x < -maxX = -x
                                            | otherwise = x
    boundsPositionY newPosition@(Point x y) | y > maxY = -y
                                            | y < -maxY = -y
                                            | otherwise = y
    maxX = int2Float ( fst (screenSize gstate)) /2
    maxY = int2Float (snd (screenSize gstate)) /2


generateRandomAsteroid :: StdGen -> (Asteroid, StdGen)
generateRandomAsteroid g =
  let
    (v, gen1) = randomR (3, 6) g
    (a, gen2) = randomR (0, 10) gen1
    (x, gen3) = randomR (0, 10) gen2
    (y, gen4) = randomR (0, 10) gen3
    (s, gen5) = randomR (30, 60) gen4
  in
    ((Asteroid {asteroidSpeed = v, asteroidDirection = Angle (a * 36), asteroidPosition = Point (x*40) (y*40), asteroidSize = s}), gen5)

generateRandomAsteroidList :: StdGen -> Int -> [Asteroid]
generateRandomAsteroidList _ 0 = []
generateRandomAsteroidList g n = let (asteroid, gen1) = generateRandomAsteroid g
                                 in asteroid : generateRandomAsteroidList gen1 (n-1)