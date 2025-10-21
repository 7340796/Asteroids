-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import GHC.Float (int2Float)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures (displayScore gstate : displayLives gstate : playerCircle gstate : (asteroidCircles gstate) ++ (bulletCircles gstate) ++ (enemyCircles gstate))

playerCircle :: GameState -> Picture
playerCircle gstate = translate (x point) (y point) picture
 where
    picture = color red (circle (playerSize (player gstate)))
    point = playerPosition (player gstate)
    x (Point a _) = a
    y (Point _ b) = b 

asteroidCircles :: GameState -> [Picture]
asteroidCircles gstate = map asteroidCircle (asteroids gstate)

asteroidCircle :: Asteroid -> Picture
asteroidCircle ast = translate (x point) (y point) picture
  where
    picture = color green (circle (asteroidSize ast))
    point = asteroidPosition ast
    x (Point a _) = a
    y (Point _ b) = b

bulletCircles :: GameState -> [Picture]
bulletCircles gstate = map bulletCircle (bullets gstate)

bulletCircle :: Bullet -> Picture
bulletCircle bul = translate (x point) (y point) picture
  where
      picture = color green (circle (bulletSize bul))
      point = bulletPosition bul
      x (Point a _) = a
      y (Point _ b) = b

displayLives :: GameState -> Picture
displayLives gstate = translate (- int2Float(fst (screenSize gstate))/2) (- int2Float(snd (screenSize gstate))/2) picture
   where
    picture = color yellow( text (show (lives gstate)))

displayScore :: GameState -> Picture
displayScore gstate = translate (- int2Float(fst (screenSize gstate))/2 + 80) (- int2Float(snd (screenSize gstate))/2) picture
   where
    picture = color red( text (show (score gstate)))

enemyCircles :: GameState -> [Picture]
enemyCircles gstate = map enemyCircle (enemies gstate)

enemyCircle :: Enemy -> Picture
enemyCircle en = translate (x point) (y point) picture
  where
      picture = color red (circle (enemySize en))
      point = enemyPosition en
      x (Point a _) = a
      y (Point _ b) = b
-- color red (circle (playerSize (player gstate)))
-- color red (text (show (playerDirection (player gstate))))

{- viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c]) -}