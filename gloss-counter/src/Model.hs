-- | This module contains the data types
--   which represent the state of the game
module Model where

data Player = Player {
                playerPosition     :: Point
              , playerDirection    :: Angle
              , playerSpeed        :: Float
              , acceleration       :: Float
              , playerSize       :: Float -- grootte van de cirkel
             }
data Enemy = Enemy {
                enemyPosition  :: Point
              , enemyDirection :: Angle
              , enemySpeed     :: Float
             }
data Bullet = Bullet {
                bulletPosition  :: Point
              , bulletDirection :: Angle
              , bulletSpeed     :: Float 
             }
data Asteroid = Asteroid {
                asteroidPosition  :: Point
              , asteroidDirection :: Angle
              , asteroidSpeed     :: Float
             }
data Point  = Point Float Float 
data Vector = Vector Float Float 
newtype Angle  = Angle Float 
newtype Lives  = Lives Int
newtype Score  = Score Float
data State  = Start | Playing | Paused | GameOver


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   elapsedTime :: Float
                 , state       :: State
                 , lives       :: Lives
                 , score       :: Score 
                 , player      :: Player 
                 , asteroids   :: [Asteroid]
                 , enemies     :: [Enemy]
                 , bullets     :: [Bullet]
                 }

initialState :: GameState
initialState = GameState 0 Start (Lives 3) (Score 0) (Player (Point 0 0) (Angle 0) 0 5 50) [] [] []