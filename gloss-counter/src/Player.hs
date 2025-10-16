module Player where
import qualified Data.Set as S
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Bullet 

--Go through the list of pressed keys and update the gamestate accordingly
updatePlayer :: GameState -> GameState
updatePlayer gstate =  turn (keys gstate) (moveForward (keys gstate) gstate)

moveForward :: S.Set Key -> GameState -> GameState
moveForward s gstate | S.member (Char 'w') s = gstate{player = updatePlayerPosition (playerAccelerate (player gstate))}
                     | otherwise           = gstate{player = updatePlayerPosition (playerSlowDown (player gstate))}

turn :: S.Set Key -> GameState -> GameState
turn s gstate = foldr turnHelper gstate s

turnHelper :: Key -> GameState -> GameState
turnHelper k gstate = 
  case k of
    (Char 'a')            -> gstate {player = (player gstate){playerDirection = steerLeft (player gstate)}} 
    (Char 'd')            -> gstate {player = (player gstate){playerDirection = steerRight (player gstate)}}
    (SpecialKey KeySpace) -> gstate {bullets = (spawnBullet gstate) : (bullets gstate)}
    _ -> gstate

--Set the player speed when slowing down
playerSlowDown :: Player -> Player
playerSlowDown  p@(Player {playerSpeed = v}) = p{playerSpeed = newSpeed}
  where
    newSpeed | v > 0     = v - 0.15
             | otherwise = 0

--Set the player speed when speeding up
playerAccelerate :: Player -> Player
playerAccelerate p@(Player {playerSpeed = v}) = p{playerSpeed = newSpeed}
  where 
    newSpeed       | v < maxSpeed = v + acceleration p
                   | otherwise = v
    maxSpeed       = 10 

--Set the new player position
updatePlayerPosition :: Player -> Player
updatePlayerPosition p@(Player {playerDirection = Angle a, playerSpeed = v, playerPosition = Point x y}) = p{playerPosition = newPosition} 
  where 
    xComponent  = cos (convert a)
    yComponent  = sin (convert a)
    convert a   = a * pi / 180 --convert from degrees to radials
    newPosition = Point (xComponent * v + x) (yComponent * v + y)

--Increase and decrease player angle, bounds at 0 and 360
steerLeft :: Player -> Angle
steerLeft p@(Player {playerDirection = Angle a}) | a + 5 > 360 = Angle (a + 5 - 360)
                                                 | otherwise    = Angle (a + 5)

steerRight :: Player -> Angle
steerRight p@(Player {playerDirection = Angle a}) | a - 5 < 0 = Angle(a - 5 + 360)
                                                  | otherwise  = Angle (a - 5)