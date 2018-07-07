module Lib
    ( game
    ) where

import System.Console.ANSI
import System.IO
import System.Timeout
import Control.Concurrent
import Control.Concurrent.Async
import Coord
import Input
import World

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

oneSecond :: Int
oneSecond = (10::Int) ^ (6::Int)

inputInterval :: Int
inputInterval = div oneSecond 10

drawGame :: World -> IO()
drawGame world = do
  clearScreen
  setCursorPosition 0 0
  putStr $ show world

gameLoop :: World -> IO ()
gameLoop world = do
  drawGame world
  input <- sample inputInterval getInput
  if (currentScene world == Main && input == Just Q)
    then handleExit
    else handleInput world input

handleInput :: World -> Maybe Input -> IO()
handleInput w Nothing = gameLoop w
handleInput w@(World {currentScene=scene}) (Just i)
  | not $ isInputUseful w i = gameLoop w
  | otherwise =
    case scene of
      Main -> handleMainScene w i
      Dungeons -> handleDungeonScene w i
      HeroInfo -> handleHeroInfoScene w i
      Fight -> gameLoop w

handleMainScene :: World -> Input -> IO()
handleMainScene world inp = gameLoop nw
  where nw = case inp of
          D -> world{currentScene = Dungeons}
          H -> world{currentScene = HeroInfo}

handleDungeonScene :: World -> Input -> IO()
handleDungeonScene world inp = gameLoop nw
  where nw = case inp of
          M -> world{currentScene = Main}

handleHeroInfoScene :: World -> Input -> IO()
handleHeroInfoScene world inp = gameLoop nw
  where nw = case inp of
          M -> world{currentScene = Main}

game :: IO ()
game = do
  gameInit
  gameLoop $ World {
    currentScene = Main,
    heros = [Hero "h1" 1 1],
    dungeons = [Dungeon "d1" [Enemy "e1" 1 2 3 4]],
    wHero = (0,0)}

handleExit :: IO ()
handleExit = do
  clearScreen
  setCursorPosition 0 0
  setSGR [Reset]
  showCursor
  putStrLn "exit game"

-- THe threadDelay is used so that the screen does not Blink
sample :: Int -> IO a -> IO (Maybe a)
sample l f
  | l < 0 = fmap Just f
  | l == 0 = return Nothing
  | otherwise = concurrently (timeout l f) (threadDelay l)
                >>= \ (result, _) -> return result
