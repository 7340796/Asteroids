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
import BoundingBox (doesIntersect)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = do 
     return $ checkForCollisions (updateBullets (updateAsteroids (updatePlayer gstate))){elapsedTime = (elapsedTime gstate) + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

--Add a key to the set of pressed keys when a key is pressed. Removes when key is released.
handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) gstate = gstate {keys = S.insert k (keys gstate)}
handleInput (EventKey k Up _ _) gstate   = gstate {keys = S.delete k (keys gstate)}
handleInput _ gstate                     = gstate

checkForCollisions :: GameState -> GameState
checkForCollisions gstate = gstate{asteroids = newAsteroidList'}--, enemies = newEnemyList} --lives = newLives}
  where
    newAsteroidList | null $ bullets gstate = asteroids gstate
                    | otherwise = [x | x <- asteroids gstate, y <- bullets gstate, not $ bulletHitsAsteroid y x]
    newEnemyList    | null $ bullets gstate = enemies gstate
                    | otherwise =  [x | x <- enemies gstate  , y <- bullets gstate, not $ bulletHitsEnemy y x]
    --newLives        | or $ [bulletHitsPlayer x (player gstate) | x <- bullets gstate] = lives gstate - 1
    --                | otherwise = lives gstate
    newAsteroidList' = [x | x <- asteroids gstate, not $ playerHitsAsteroid (player gstate) x]

bulletHitsAsteroid :: Bullet -> Asteroid -> Bool
bulletHitsAsteroid bul ast = doesIntersect bulHitbox astHitbox
  where 
    bulHitbox = getHitbox bul
    astHitbox = getHitbox ast 

bulletHitsEnemy :: Bullet -> Enemy -> Bool
bulletHitsEnemy bul e = doesIntersect bulHitbox eHitbox
  where
    bulHitbox = getHitbox bul
    eHitbox   = undefined

bulletHitsPlayer :: Bullet -> Player -> Bool
bulletHitsPlayer bul p = doesIntersect bulHitbox pHitbox
  where
    bulHitbox = getHitbox bul
    pHitbox   = getHitbox p

playerHitsAsteroid :: Player -> Asteroid -> Bool
playerHitsAsteroid p ast = doesIntersect pHitbox astHitbox
  where 
    pHitbox   = getHitbox p
    astHitbox = getHitbox ast