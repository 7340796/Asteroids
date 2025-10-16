module BoundingBox where
import Model

calculateDistance :: Point -> Point -> Float
calculateDistance p1@(Point x y) p2@(Point a b) = sqrt (deltaXSquared + deltaYSquared)
    where
        deltaXSquared = (a - x) * (a - x)
        deltaYSquared = (b - y) * (b - y)

doesIntersect :: HitBox -> HitBox -> Bool
doesIntersect (HitBox s1 p1) (HitBox s2 p2) = distance <= 0.1
    where 
        distance = (calculateDistance p1 p2) - combinedSize
        combinedSize = s1 + s2