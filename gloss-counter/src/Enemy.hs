module Enemy where

import Model
import GHC.Float (int2Float)

instance Entity Enemy where
  updatePosition = updateEnemyPosition
  getHitbox e    = HitBox (enemySize e) (enemyPosition e)

updateEnemies :: GameState -> GameState
updateEnemies gstate | null (enemies gstate) = gstate{enemies = spawnEnemy gstate : updatedEnemies}
                     | otherwise             = gstate{enemies = updatedEnemies}
  where 
    updatedEnemies = map (\x -> updateEnemyPosition x gstate) (enemies gstate)

updateEnemyPosition :: Enemy -> GameState -> Enemy
updateEnemyPosition en@(Enemy {enemyPosition = enemyPosition, enemyDirection = Angle a, enemySpeed = v}) gstate = en{enemyPosition = newPosition, enemyDirection = Angle newDirection}
    where 
      playerPos = playerPosition (player gstate)
      deltaX = abs (x playerPos - x enemyPosition)
      deltaY = abs (y playerPos - y enemyPosition)
      lengthC = sqrt(deltaX * deltaX + deltaY * deltaY)
      newDirection = acos(deltaY * deltaY + lengthC * lengthC - (deltaX * deltaX)) / (2 * deltaY * lengthC) --cosine rule
      x (Point x y)     = x
      y (Point x y)     = y
      xComponent  = cos (convert a)
      yComponent  = sin (convert a)
      convert a   = a * pi / 180 --convert from degrees to radials
      newPosition = Point (xComponent * v + x enemyPosition) (yComponent * v + y enemyPosition)



spawnEnemy :: GameState -> Enemy
spawnEnemy gstate = Enemy {enemyPosition = spawnPosition, enemyDirection = Angle 90, enemySpeed = 8, enemySize = 20}
   where
    xSpawn = 40 --(int2Float(fst (screenSize gstate)) / 2)
    ySpawn = 40 -- (int2Float(snd (screenSize gstate)) / 2)
    spawnPosition = Point xSpawn ySpawn