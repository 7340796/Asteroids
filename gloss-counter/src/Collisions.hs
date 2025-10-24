module Collisions where
import Model
import BoundingBox (doesIntersect)
import Data.List
import Player
import Bullet
import Enemy
import Asteroid

--newList gives back all items that did not collide. If the player collides with something, they lose a life. When all lives are lost, the gamestate is changed to the gameover state.
checkForCollisions :: GameState -> GameState
checkForCollisions gstate | lives gstate > 0 = gstate{asteroids = newAsteroidList, bullets = newBulletList, score = newScore, lives = newLives, enemies = newEnemyList, player = newPlayer}
                          | otherwise = gstate{state = GameOver}
  where
    newAsteroidList = filter (\x -> not $ any (\y -> bulletHitsAsteroid y x) (bullets gstate)) (asteroids gstate) \\ filter (playerHitsAsteroid (player gstate)) (asteroids gstate)
    newBulletList   = filter (\x -> not $ any (\y -> bulletHitsAsteroid x y || bulletHitsPlayer x (player gstate)) (asteroids gstate)) (bullets gstate)
    newScore        | length newAsteroidList < length (asteroids gstate) = (score gstate) + 20
                    | length newEnemyList < length (enemies gstate)      = (score gstate) + 20
                    | otherwise                                          = score gstate
    newLives        | any (\x -> bulletHitsPlayer x (player gstate)) (bullets gstate) || any (playerHitsAsteroid (player gstate)) (asteroids gstate) || any (playerHitsEnemy (player gstate)) (enemies gstate) = lives gstate - 1
                    | otherwise = lives gstate
    newPlayer       | newLives < lives gstate = (player gstate){playerPosition = Point 0 0, playerDirection = Angle 90, playerSpeed = 0}
                    | otherwise               = player gstate  
    newEnemyList    = filter (\x -> not $ any (\y -> bulletHitsEnemy y x) (bullets gstate)) (enemies gstate)

bulletHitsAsteroid :: Bullet -> Asteroid -> Bool
bulletHitsAsteroid bul ast = doesIntersect bulHitbox astHitbox
  where
    bulHitbox = getHitbox bul
    astHitbox = getHitbox ast

bulletHitsPlayer :: Bullet -> Player -> Bool
bulletHitsPlayer bul p = doesIntersect bulHitbox pHitbox
  where
    bulHitbox = getHitbox bul
    pHitbox   = getHitbox p

bulletHitsEnemy :: Bullet -> Enemy -> Bool
bulletHitsEnemy bul e = doesIntersect bulHitbox eHitbox
  where
    bulHitbox = getHitbox bul
    eHitbox   = getHitbox e

playerHitsAsteroid :: Player -> Asteroid -> Bool
playerHitsAsteroid p ast = doesIntersect pHitbox astHitbox
  where
    pHitbox   = getHitbox p
    astHitbox = getHitbox ast

playerHitsEnemy :: Player -> Enemy -> Bool
playerHitsEnemy p e = doesIntersect pHitbox eHitbox
  where
    pHitbox = getHitbox p
    eHitbox = getHitbox e