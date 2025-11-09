module Enemy where

import Model
import GHC.Float (int2Float, acos, float2Int)
import Bullet
import Data.Maybe (mapMaybe)
import Entity

instance Entity Enemy where
  getHitbox e    = HitBox (enemySize e) (enemyPosition e)
  position       = enemyPosition
  speed          = enemySpeed
  size           = enemySize
  direction      = enemyDirection


--Add all shot bullets to the gamestate bullet list
shootForAll :: GameState -> GameState
shootForAll gstate@GameState{bullets = bullets} = gstate{bullets = bullets ++ enemyBullets}
  where
    enemyBullets = mapMaybe shootWhenTimer (enemies gstate)
--Spawns a new enemyBullet when the timer of the enemy reaches 2 seconds
shootWhenTimer :: Enemy -> Maybe Bullet
shootWhenTimer e@Enemy{shootTimer = shootTimer} | shootTimer `mod` 60 == 0 = Just (spawnEnemyBullet e)
                                                | otherwise                = Nothing

updateTimer :: Enemy -> Enemy
updateTimer e@Enemy{shootTimer = shootTimer} = e{shootTimer = shootTimer + 1}

--either spawn a new enemy when the player has shot all the asteroids, or update the enemies
updateEnemies :: GameState -> GameState
updateEnemies gstate | null (asteroids gstate) && elapsedTime gstate > 0.1 = gstate{enemies = spawnEnemy gstate : updatedEnemies}
                     | otherwise                                           = shootForAll gstate{enemies = updatedEnemies}
  where
    updatedEnemies = map (\x ->  updateTimer (updateEnemyPosition (updateEnemyDirection x gstate) gstate)) (enemies gstate)

updateEnemyPosition :: Enemy -> GameState -> Enemy
updateEnemyPosition en gstate = en{enemyPosition = updatePosition' en gstate}

--Follow the player
updateEnemyDirection :: Enemy -> GameState -> Enemy
updateEnemyDirection en@(Enemy {enemyDirection = Angle a, enemyPosition = enemyPos}) gstate = en{enemyDirection = Angle newDirection}
  where
    newDirection  = atan2 deltaY deltaX * (180 / pi)
    playerPos     = playerPosition (player gstate)
    x (Point a _) = a
    y (Point _ b) = b
    deltaX        = x playerPos - x enemyPos
    deltaY        = y playerPos - y enemyPos
    lengthC       = sqrt (deltaX * deltaX + deltaY * deltaY)

spawnEnemy :: GameState -> Enemy
spawnEnemy gstate = Enemy {enemyPosition = spawnPosition, enemyDirection = Angle 90, enemySpeed = 2, enemySize = 20, shootTimer = 0}
   where
    xSpawn        =  int2Float (fst (screenSize gstate)) / 2
    ySpawn        =  int2Float (snd (screenSize gstate)) / 2
    spawnPosition = Point xSpawn ySpawn