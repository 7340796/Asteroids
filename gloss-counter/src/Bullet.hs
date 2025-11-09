module Bullet where
import Model
import GHC.Float
import Entity

instance Entity Bullet where
  getHitbox bul  = HitBox (bulletSize bul) (bulletPosition bul)
  position       = bulletPosition
  size           = bulletSize
  direction      = bulletDirection
  speed          = bulletSpeed

updateBullets :: GameState -> GameState
updateBullets gstate = gstate{bullets = updatedBullets}
  where
    updatedBullets = map (`updateBulletPosition` gstate) (bullets (updateLife gstate))

updateBulletPosition :: Bullet -> GameState-> Bullet
updateBulletPosition bul gstate = bul{bulletPosition = boundsPosition' updatedbullet gstate}
  where
    updatedbullet = bul{bulletPosition = updatePosition' bul gstate}

--Spawn a bullet with the same direction the player is looking
spawnPlayerBullet :: GameState -> Bullet
spawnPlayerBullet gstate = Bullet {bulletPosition = spawnPosition, bulletDirection = playerDirection player', bulletSpeed = 18, bulletSize = 5, lifeTime = 30}
    where
        player'           = player gstate
        spawnDirection    = playerDirection (player gstate)
        convert (Angle a) = a * pi / 180
        spawnPosition     = Point (xComponent * playerSize (player gstate) + x (playerPosition (player gstate))) (yComponent * playerSize (player gstate) + y (playerPosition (player gstate)))
        xComponent        = cos (convert spawnDirection)
        yComponent        = sin (convert spawnDirection)
        x (Point x y)     = x
        y (Point x y)     = y

spawnEnemyBullet :: Enemy -> Bullet
spawnEnemyBullet enemy = Bullet {bulletPosition = spawnPosition, bulletDirection = spawnDirection, bulletSpeed = 18, bulletSize = 5, lifeTime = 30}
    where
        spawnDirection    = enemyDirection enemy
        convert (Angle a) = a * pi / 180
        spawnPosition     = Point (xComponent * enemySize enemy + x (enemyPosition enemy)) (yComponent * enemySize enemy + y (enemyPosition enemy))
        xComponent        = cos (convert spawnDirection)
        yComponent        = sin (convert spawnDirection)
        x (Point x y)     = x
        y (Point x y)     = y

--Lowers the lifetime of a bullet with 1 each frame. This way, bullets don't fly around forever.
updateLife :: GameState -> GameState
updateLife gstate = gstate{bullets = filter isAlive updatedBullets}
  where
    isAlive bul        = lifeTime bul > 0
    updatedBullets     = map updateLifeTime (bullets gstate)
    updateLifeTime bul = bul{lifeTime = lifeTime bul - 1}