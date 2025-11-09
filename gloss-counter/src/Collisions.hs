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
checkForCollisions gstate | lives gstate > 0 = gstate{asteroids = newAsteroidList, bullets = newBulletList, score = newScore, lives = newLives, enemies = newEnemyList, player = newPlayer, deadAsteroids = newDeadAsteroidList}
                          | otherwise = gstate{state = GameOver}
  where
    newAsteroidList = fst (asteroidCollisionDetector (asteroids gstate) gstate)
    newDeadAsteroidList = deadAsteroids gstate ++ snd (asteroidCollisionDetector (asteroids gstate) gstate)
    newBulletList   = bulletCollisionDetector (bullets gstate) gstate
    newScore        | length newAsteroidList < length (asteroids gstate) = (score gstate) + 20
                    | length newEnemyList < length (enemies gstate)      = (score gstate) + 20
                    | otherwise                                          = score gstate
    newLives        | any (\x -> collidesWith x (player gstate)) (bullets gstate) || any (collidesWith (player gstate)) (asteroids gstate) || any (collidesWith (player gstate)) (enemies gstate) = lives gstate - 1
                    | otherwise = lives gstate
    newPlayer       | newLives < lives gstate = (player gstate){playerPosition = Point 0 0, playerDirection = Angle 90, playerSpeed = 0, animationTimer = 0}
                    | otherwise               = player gstate  
    newEnemyList    = filter (\x -> not $ any (\y -> collidesWith y x) (bullets gstate)) (enemies gstate)

--Remove all asteroids that get hit from the asteroid list
asteroidCollisionDetector :: [Asteroid] -> GameState -> ([Asteroid], [Asteroid])
asteroidCollisionDetector asts gstate = ((asts \\ (nub (collidesWithPlayer ++ collidesWithBullets))), (nub (collidesWithPlayer ++ collidesWithBullets)))
  where
    collidesWithPlayer  = mapMaybe (\x -> acdHelper x [player gstate] gstate) asts  
    collidesWithBullets = mapMaybe (\x -> acdHelper x (bullets gstate) gstate) asts
--Returns the asteroid if it gets hit by the entity
acdHelper :: Entity e => Asteroid -> [e] -> GameState -> Maybe Asteroid
acdHelper ast es gstate | none (collidesWith ast) es = Nothing
                        | otherwise                  = Just ast

bulletCollisionDetector :: [Bullet] -> GameState -> [Bullet]
bulletCollisionDetector buls gstate = (buls \\ (nub (collidesWithPlayer ++ collidesWithEnemy ++ collidesWithAsteroid)))
  where
    collidesWithPlayer   = mapMaybe (\x -> bcdHelper x [player gstate] gstate) buls
    collidesWithAsteroid = mapMaybe (\x -> bcdHelper x (asteroids gstate) gstate) buls
    collidesWithEnemy    = mapMaybe (\x -> bcdHelper x (enemies gstate) gstate) buls
  
bcdHelper :: Entity e => Bullet -> [e] -> GameState -> Maybe Bullet
bcdHelper bul es gstate | none (collidesWith bul) es = Nothing
                         | otherwise                 = Just bul
