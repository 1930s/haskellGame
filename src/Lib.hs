module Lib
    ( game
    ) where

import System.Console.ANSI
import System.IO

type Coord = (Int, Int)

data World = World {wHero :: Coord}

data Input = North
           | South
           | West
           | East
           | Enter
           | Exit
           deriving (Eq, Show)

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

(|+|) :: Coord -> Coord -> Coord
(|+|) (a,b) (x,y) = (a+x, b+y)

minCoord :: Coord
minCoord = (0,0)

maxCoord :: Coord
maxCoord = (80,80)

restrictCoord :: Coord -> Coord -> Coord -> Coord
restrictCoord (x,y) (xMin, yMin) (xMax, yMax) = (res x xMin xMax, res y yMin yMax)
  where res v vMin vMax = max vMin $ min v vMax

inputToDir :: Input -> Coord
inputToDir i =
  case i of
    North -> (0,-1)
    South -> (0,1)
    West -> (-1,0)
    East -> (1,0)
    _ -> (0,0)

getInput :: IO (Input)
getInput = do
  key <- getChar
  case key of
    'w' -> return North
    's' -> return South
    'a' -> return West
    'd' -> return East
    'q' -> return Exit
    _ -> getInput

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
