module Bullet where
import Model
import GHC.Float

updateBullets :: GameState -> GameState
updateBullets gstate = gstate{bullets = updatedBullets}
  where 
    updatedBullets = map (\x -> updateBulletPosition x gstate) (bullets gstate)

instance Entity Bullet where
  updatePosition = updateBulletPosition
  getHitbox bul  = HitBox (bulletSize bul) (bulletPosition bul)

updateBulletPosition :: Bullet -> GameState-> Bullet
updateBulletPosition bul@(Bullet {bulletSpeed = v, bulletDirection = Angle a, bulletPosition = Point x y}) gstate = bul{bulletPosition = Point (boundsPositionX newPosition) (boundsPositionY newPosition)}
  where
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180       --convert from degrees to radials
    newPosition = Point (xComponent * v + x) (yComponent * v + y)
    boundsPositionX newPosition@(Point x y) | x > maxX = -x
                                            | x < -maxX = -x
                                            | otherwise = x
    boundsPositionY newPosition@(Point x y) | y > maxY = -y
                                            | y < -maxY = -y
                                            | otherwise = y
    maxX = int2Float ( fst (screenSize gstate)) /2
    maxY = int2Float (snd (screenSize gstate)) /2

spawnBullet :: GameState -> Bullet
spawnBullet gstate = Bullet {bulletPosition = playerPosition player', bulletDirection = playerDirection player', bulletSpeed = 10, bulletSize = 5}
    where 
        player' = player gstate 


