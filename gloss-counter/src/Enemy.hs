module Enemy where

import Model
import GHC.Float (int2Float, acos, float2Int)
import Bullet
import Data.Maybe (mapMaybe)

instance Entity Enemy where
  updatePosition = updateEnemyPosition
  getHitbox e    = HitBox (enemySize e) (enemyPosition e)

shootForAll :: GameState -> GameState
shootForAll gstate@GameState{bullets = bullets} = gstate{bullets = bullets ++ enemyBullets}
  where
    enemyBullets = mapMaybe (shootWhenTimer) (enemies gstate)

shootWhenTimer :: Enemy -> Maybe Bullet
shootWhenTimer e@Enemy{shootTimer = shootTimer} | shootTimer `mod` 60 == 0 = Just (spawnEnemyBullet e)
                                       | otherwise                = Nothing

updateTimer :: Enemy -> Enemy
updateTimer e@Enemy{shootTimer = shootTimer} = e{shootTimer = shootTimer + 1}

updateEnemies :: GameState -> GameState
updateEnemies gstate | null(asteroids gstate) && elapsedTime gstate > 0 = gstate{enemies = spawnEnemy gstate : updatedEnemies}
                     | otherwise                     = shootForAll gstate{enemies = updatedEnemies}
  where 
    updatedEnemies = map (\x ->  updateTimer (updateEnemyPosition (updateEnemyDirection x gstate) gstate)) (enemies gstate)
    
updateEnemyPosition :: Enemy -> GameState -> Enemy
updateEnemyPosition en@(Enemy {enemyPosition = (Point x y), enemyDirection = Angle a, enemySpeed = v}) gstate = en{enemyPosition = newPosition}
    where 
      --playerPos = playerPosition (player gstate)
      --deltaX        = abs (x playerPos - x enemyPosition)
      --deltaY        = abs (y playerPos - y enemyPosition)
      --lengthC       = sqrt(deltaX * deltaX + deltaY * deltaY)
      --newDirection  = atan(deltaY / deltaX) --acos(deltaY * deltaY + lengthC * lengthC - (deltaX * deltaX)) / (2 * deltaY * lengthC) --cosine rule
      xComponent  = cos (convert a)
      yComponent  = sin (convert a)
      convert a   = a * pi / 180 --convert from degrees to radials
      newPosition = Point (xComponent * v + x) (yComponent * v + y)

updateEnemyDirection :: Enemy -> GameState -> Enemy
updateEnemyDirection en@(Enemy {enemyDirection = Angle a, enemyPosition = enemyPos}) gstate = en{enemyDirection = Angle newDirection}
  where
    newDirection  = atan2 deltaY deltaX * (180 / pi)
    playerPos     = playerPosition (player gstate)
    x (Point a _) = a
    y (Point _ b) = b
    deltaX        = x playerPos - x enemyPos
    deltaY        = y playerPos - y enemyPos
    lengthC       = sqrt(deltaX * deltaX + deltaY * deltaY)  


spawnEnemy :: GameState -> Enemy
spawnEnemy gstate = Enemy {enemyPosition = spawnPosition, enemyDirection = Angle 90, enemySpeed = 2, enemySize = 20, shootTimer = 0}
   where
    xSpawn =  (int2Float(fst (screenSize gstate)) / 2)
    ySpawn =  (int2Float(snd (screenSize gstate)) / 2)
    spawnPosition = Point xSpawn ySpawn