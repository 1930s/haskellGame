module Lib
    ( game
    ) where

import System.Console.ANSI
import System.IO
import Input
import Coord

data World = World {wHero :: Coord}

gameInit :: IO ()
gameInit = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "myRogue"
  setSGR [SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue]

drawHero :: Coord -> IO ()
drawHero (x,y) = do
  clearScreen
  setCursorPosition y x
  putStr "@"

gameLoop :: World -> IO ()
gameLoop world = do
  drawHero $ wHero world
  input <- getInput
  case input of
    Exit -> handleExit
    _ -> handleInput world input

handleInput :: World -> Input -> IO()
handleInput w i = gameLoop $ World {wHero = nw}
  where
    hPos = wHero w
    nw = restrictCoord nw_unrestricted minCoord maxCoord
    nw_unrestricted = inputToDir i |+| hPos

game :: IO ()
game = do
  gameInit
  gameLoop $ World (0,0)

handleExit :: IO ()
handleExit = do
  clearScreen
  setCursorPosition 0 0
  setSGR [Reset]
  showCursor
  putStrLn "exit game"
