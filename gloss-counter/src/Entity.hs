module Entity where
import GHC.Float (int2Float)
import Model
import BoundingBox

updatePosition' :: Entity e => e -> GameState -> Point
updatePosition' e gstate = Point (xComponent * v + x) (yComponent * v + y)
  where
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180
    Angle a     = direction e
    v           = speed e
    Point x y   = position e 

boundsPosition' :: Entity e => e -> GameState -> Point
boundsPosition' e gstate = Point (boundsPositionX (position e)) (boundsPositionY (position e))
  where
    boundsPositionX newPosition@(Point x y) | x > maxX = -x
                                            | x < -maxX = -x
                                            | otherwise = x
    boundsPositionY newPosition@(Point x y) | y > maxY = -y
                                            | y < -maxY = -y
                                            | otherwise = y
    maxX = int2Float ( fst (screenSize gstate)) /2
    maxY = int2Float (snd (screenSize gstate)) /2

collidesWith :: (Entity e, Entity e') => e -> e' -> Bool
collidesWith e1 e2 = doesIntersect (getHitbox e1) (getHitbox e2)  