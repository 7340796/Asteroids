module Collisions where
import Model
import BoundingBox (doesIntersect)
import Data.List
import Player
import Bullet
import Enemy
import Asteroid
import Entity
import Data.Set (fromList, toList)
import Data.Express.Utils.List (none, nub)
import Data.Maybe

--newList gives back all items that did not collide. If the player collides with something, they lose a life. When all lives are lost, the gamestate is changed to the gameover state.
checkForCollisions :: GameState -> GameState
checkForCollisions gstate | lives gstate > 0 = gstate{asteroids = newAsteroidList, bullets = newBulletList, score = newScore, lives = newLives, enemies = newEnemyList, player = newPlayer}
                          | otherwise = gstate{state = GameOver}
  where
    newAsteroidList = foo (asteroids gstate) gstate --filter (\x -> not $ any (\y -> collidesWith y x) (bullets gstate)) (asteroids gstate) \\ filter (collidesWith (player gstate)) (asteroids gstate)
    newBulletList   = filter (\x -> not $ any (\y -> collidesWith x y || collidesWith x (player gstate)) (asteroids gstate)) (bullets gstate)
    newScore        | length newAsteroidList < length (asteroids gstate) = (score gstate) + 20
                    | length newEnemyList < length (enemies gstate)      = (score gstate) + 20
                    | otherwise                                          = score gstate
    newLives        | any (\x -> collidesWith x (player gstate)) (bullets gstate) || any (collidesWith (player gstate)) (asteroids gstate) || any (collidesWith (player gstate)) (enemies gstate) = lives gstate - 1
                    | otherwise = lives gstate
    newPlayer       | newLives < lives gstate = (player gstate){playerPosition = Point 0 0, playerDirection = Angle 90, playerSpeed = 0, animationTimer = 0}
                    | otherwise               = player gstate  
    newEnemyList    = filter (\x -> not $ any (\y -> collidesWith y x) (bullets gstate)) (enemies gstate)

--Remove all asteroids that get hit from the asteroid list
foo :: [Asteroid] -> GameState -> [Asteroid]
foo asts gstate = asts \\ (nub (collidesWithPlayer ++ collidesWithBullets))
  where
    collidesWithPlayer  = mapMaybe (\x -> fooHelper x [player gstate]) asts
    collidesWithBullets = mapMaybe (\x -> fooHelper x (bullets gstate)) asts

--Returns the asteroid if it gets hit by the entity
fooHelper :: Entity e => Asteroid -> [e] -> Maybe Asteroid
fooHelper ast es = case (predicate) of 
                    True  -> Nothing
                    False -> do
                               --Ik dacht dat we dan hier de asteroid aan een set of lijst kunnen toevoegen, zodat je er over meerdere frames bij kan in bijv. de animation class
                               --Dan kan je daar een functie animateAsteroid maken, en die mappen over de lijst, zodat je voor elke dode asteroid de animatie update.
                               --Als de animatie dan voorbij is, kan je hem uit de lijst halen en is ie officieel dood. Vond ik wel een goed idee.
                             Just ast
  where
    predicate = none (collidesWith ast) es --True when no entity hits the asteroid

  