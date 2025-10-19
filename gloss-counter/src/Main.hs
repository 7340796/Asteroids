module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Random (getStdGen)
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
    gen <- getStdGen
    screenSize <- getScreenSize
    playIO FullScreen -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              (initialState gen screenSize)     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function