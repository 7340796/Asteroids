-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = translate (x point) (y point) picture
  where
    picture = color red (circle (playerSize (player gstate)))
    point = playerPosition (player gstate)
    x (Point a _) = a
    y (Point _ b) = b 




{- viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c]) -}