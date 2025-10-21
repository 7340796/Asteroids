-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Set as S -- import datatype Set
import Player
import Asteroid
import Bullet
import Enemy
import BoundingBox (doesIntersect)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = do
     return $ checkForCollisions (updateEnemies(updateBullets (updateAsteroids (updatePlayer gstate)))){elapsedTime = (elapsedTime gstate) + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

--Add a key to the set of pressed keys when a key is pressed. Removes when key is released.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate {keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate   = gstate {keys = S.delete k (keys gstate)}
handleInput _ gstate                     = gstate

checkForCollisions :: GameState -> GameState
checkForCollisions gstate | lives gstate > 0 = gstate{asteroids = newAsteroidList, bullets = newBulletList, score = newScore, lives = newLives}--, enemies = newEnemyList} --
                          | otherwise = gstate{state = GameOver}
  where
    newAsteroidList = filter (\x -> not $ any (\y -> bulletHitsAsteroid y x) (bullets gstate)) (asteroids gstate) \\ filter (playerHitsAsteroid (player gstate)) (asteroids gstate)
    newBulletList   = filter (\x -> not $ any (\y -> bulletHitsAsteroid x y || bulletHitsPlayer x (player gstate)) (asteroids gstate)) (bullets gstate)
    newScore        | length newAsteroidList < length (asteroids gstate) = (score gstate) + 20
                    | otherwise                                          = score gstate
    newLives        | any (\x -> bulletHitsPlayer x (player gstate)) (bullets gstate) || any (playerHitsAsteroid (player gstate)) (asteroids gstate) = lives gstate - 1
                    | otherwise = lives gstate
    -- newEnemyList    | null $ bullets gstate = enemies gstate
    --                 | otherwise =  [x | x <- enemies gstate  , y <- bullets gstate, not $ bulletHitsEnemy y x]

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
-- bulletHitsEnemy :: Bullet -> Enemy -> Bool
-- bulletHitsEnemy bul e = doesIntersect bulHitbox eHitbox
--   where
--     bulHitbox = getHitbox bul
--     eHitbox   = undefined


playerHitsAsteroid :: Player -> Asteroid -> Bool
playerHitsAsteroid p ast = doesIntersect pHitbox astHitbox
  where
    pHitbox   = getHitbox p
    astHitbox = getHitbox ast
