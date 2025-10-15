-- | This module contains the data types
--   which represent the state of the game
module Model where
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game (Key)
import System.Random (StdGen)

data Player = Player {
                playerPosition     :: Point
              , playerDirection    :: Angle
              , playerSpeed        :: Float
              , acceleration       :: Float
              , playerSize         :: Float -- grootte van de cirkel
             }
  deriving(Show)
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
              , asteroidSize      :: Float 
             }
data Point  = Point Float Float
  deriving (Show)
data Vector = Vector Float Float 
newtype Angle  = Angle Float
  deriving (Show) 
newtype Lives  = Lives Int
newtype Score  = Score Float
data State  = Start | Playing | Paused | GameOver
data Direction = North | East | South | West


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
                 , keys        :: S.Set Key
                 , rg          :: StdGen
                 }

initialState :: StdGen -> GameState
initialState gen = GameState { elapsedTime = 0, state = Start, lives = Lives 3, score = Score 0, player = (Player { playerPosition = Point 0 0, playerDirection = Angle 90, playerSpeed = 0, acceleration = 1, playerSize = 50 }), asteroids = asteroidlist, enemies = [], bullets = [], keys = S.empty, rg = gen }
  where 
    asteroidlist = []