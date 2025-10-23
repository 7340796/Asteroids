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
              , enemySize      :: Float
             }
data Bullet = Bullet {
                bulletPosition  :: Point
              , bulletDirection :: Angle
              , bulletSpeed     :: Float 
              , bulletSize      :: Float
              , lifeTime        :: Int
             }
data Asteroid = Asteroid {
                asteroidPosition  :: Point
              , asteroidDirection :: Angle
              , asteroidSpeed     :: Float
              , asteroidSize      :: Float 
             } 
  deriving (Eq)
data Point  = Point Float Float
  deriving (Show, Eq)
data Vector = Vector Float Float 
newtype Angle  = Angle Float
  deriving (Show, Eq) 
data State  = Start | Playing | Paused | GameOver
data HitBox = HitBox Float Point


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   elapsedTime :: Float
                 , state       :: State
                 , lives       :: Int
                 , score       :: Int 
                 , player      :: Player 
                 , asteroids   :: [Asteroid]
                 , enemies     :: [Enemy]
                 , bullets     :: [Bullet]
                 , keys        :: S.Set Key
                 , rg          :: StdGen
                 , screenSize  :: (Int, Int)
                 }

initialState :: StdGen -> (Int, Int) -> GameState
initialState gen screenSize = GameState { elapsedTime = 0, state = Start, lives = 300, score = 0, player = (Player (Point 0 0) (Angle 90) 0 1 50), asteroids = asteroidlist, enemies = [], bullets = [], keys = S.empty, rg = gen, screenSize = screenSize }
  where 
    asteroidlist = []
    enemyList = []


class Entity a where
  updatePosition :: a -> GameState-> a
  getHitbox :: a -> HitBox
