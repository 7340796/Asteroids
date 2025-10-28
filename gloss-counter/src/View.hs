-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import GHC.Float (int2Float)
import Animations

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case state gstate of
                    Playing  -> pictures gamePicture
                    GameOver -> pictures (gameOver : gamePictureWOPlayer)
                    Paused   -> pictures (paused : gamePicture)
  where
    gamePicture = playerDirectionIndicator (player gstate): displayScore gstate :displayLives gstate : playerCircle gstate : asteroidCircles gstate ++ bulletCircles gstate ++ enemyCircles gstate
    gamePictureWOPlayer = playerCircle gstate : displayScore gstate :displayLives gstate : asteroidCircles gstate ++ bulletCircles gstate ++ enemyCircles gstate
    gameOver    = translate translateX (translateY - 120) (color red (text "Game Over! :("))
    paused      = translate translateX (translateY - 120) (color red (text "Paused..."))
    translateX = -(int2Float (fst (screenSize gstate)) / 2)
    translateY = int2Float (snd (screenSize gstate)) / 2

playerCircle :: GameState -> Picture
playerCircle gstate = translate (x point) (y point) picture
 where
    picture = color rose (circle (playerSize (player gstate)))
    point = playerPosition (player gstate)
    x (Point a _) = a
    y (Point _ b) = b

asteroidCircles :: GameState -> [Picture]
asteroidCircles gstate = map asteroidCircle (asteroids gstate)

asteroidCircle :: Asteroid -> Picture
asteroidCircle ast = translate (x point) (y point) picture
  where
    picture = color white (circle (asteroidSize ast))
    point = asteroidPosition ast
    x (Point a _) = a
    y (Point _ b) = b

bulletCircles :: GameState -> [Picture]
bulletCircles gstate = map bulletCircle (bullets gstate)

bulletCircle :: Bullet -> Picture
bulletCircle bul = translate (x point) (y point) picture
  where
      picture = color yellow (circle (bulletSize bul))
      point = bulletPosition bul
      x (Point a _) = a
      y (Point _ b) = b

displayLives :: GameState -> Picture
displayLives gstate = translate (- (int2Float (fst (screenSize gstate)) / 2)) (- (int2Float (snd (screenSize gstate)) / 2)) picture
   where
    picture = color yellow ( text (show (lives gstate)))

displayScore :: GameState -> Picture
displayScore gstate = translate (- (int2Float (fst (screenSize gstate)) / 2) + 80) (- (int2Float (snd (screenSize gstate)) / 2)) picture
   where
    picture = color red ( text (show (score gstate)))

enemyCircles :: GameState -> [Picture]
enemyCircles gstate = map enemyCircle (enemies gstate)

enemyCircle :: Enemy -> Picture
enemyCircle en = translate (x point) (y point) picture
  where
      picture = color red (circle (enemySize en))
      point = enemyPosition en
      x (Point a _) = a
      y (Point _ b) = b

playerDirectionIndicator :: Player -> Picture
playerDirectionIndicator p = translate (x point) (y point) picture
 where
    picture = color yellow (circle 4)
    point = spawnPosition
    spawnDirection    = playerDirection p
    convert (Angle a) = a * pi / 180
    spawnPosition     = Point (xComponent * playerSize p + x (playerPosition p)) (yComponent * playerSize p + y (playerPosition p))
    xComponent        = cos (convert spawnDirection)
    yComponent        = sin (convert spawnDirection)
    x (Point a _) = a
    y (Point _ b) = b
