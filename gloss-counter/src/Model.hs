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
              , playerSize         :: Float
              , animationTimer     :: Float 
             }
  deriving(Show, Eq)
data Enemy = Enemy {
                enemyPosition  :: Point
              , enemyDirection :: Angle
              , enemySpeed     :: Float
              , enemySize      :: Float
              , shootTimer     :: Int
             }
  deriving(Show, Eq) 
data Bullet = Bullet {
                bulletPosition  :: Point
              , bulletDirection :: Angle
              , bulletSpeed     :: Float 
              , bulletSize      :: Float
              , lifeTime        :: Int
             }
  deriving(Show, Eq)
data Asteroid = Asteroid {
                asteroidPosition  :: Point
              , asteroidDirection :: Angle
              , asteroidSpeed     :: Float
              , asteroidSize      :: Float 
             } 
  deriving (Show, Eq)
data Point  = Point Float Float
  deriving (Show, Eq)
data Vector = Vector Float Float
  deriving (Show, Eq) 
newtype Angle  = Angle Float
  deriving (Show, Eq) 
data State  = Playing | Paused | GameOver
  deriving (Show, Eq)
data HitBox = HitBox Float Point
  deriving (Show, Eq)

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
                 , toggleKeys  :: S.Set Key
                 , rg          :: StdGen
                 , screenSize  :: (Int, Int)
                 }

initialState :: StdGen -> (Int, Int) -> GameState
initialState gen screenSize = GameState { elapsedTime = 0, state = Playing, lives = 3, score = 0, player = player, asteroids = [], enemies = [], bullets = [], keys = S.empty, toggleKeys = S.empty, rg = gen, screenSize = screenSize }
  where 
    player = Player (Point 0 0) (Angle 90) 0 1 30 0


class Entity a where
  updatePosition :: a -> GameState-> a
  getHitbox :: a -> HitBox