module Bullet where
import Model

updateBullets :: GameState -> GameState
updateBullets gstate = gstate{bullets = updatedBullets}
  where 
    updatedBullets = map updateBulletPosition (bullets gstate)

instance Entity Bullet where
  updatePosition = updateBulletPosition
  getHitbox bul  = HitBox (bulletSize bul) (bulletPosition bul)

updateBulletPosition :: Bullet -> Bullet
updateBulletPosition bul@(Bullet {bulletSpeed = v, bulletDirection = Angle a, bulletPosition = Point x y}) = bul{bulletPosition = newPosition}
  where
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180       --convert from degrees to radials
    newPosition = Point (xComponent * v + x) (yComponent * v + y)

spawnBullet :: GameState -> Bullet
spawnBullet gstate = Bullet {bulletPosition = playerPosition player', bulletDirection = playerDirection player', bulletSpeed = 10, bulletSize = 5}
    where 
        player' = player gstate 


