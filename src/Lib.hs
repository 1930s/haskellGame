{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( game
    ) where

import System.Console.ANSI
import System.IO
import System.Random
import System.Timeout
import Control.Concurrent
import Control.Concurrent.Async
import Coord
import Input
import Core.World
import HandleInput

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
gameLoop world_ = do
  let world = gameTick world_
  drawGame world
  input <- sample inputInterval getInput
  if (currentScene world == Main && input == Just Q)
    then handleExit
    else handleInput world input

handleInput :: World -> Maybe Input -> IO()
handleInput w Nothing = gameLoop w
handleInput w@(World {currentScene=scene}) (Just i)
  | not $ isInputUseful w i = putStrLn "Invalid input" >> gameLoop w
  | otherwise =
    case scene of
      Main -> gameLoop $ handleMainScene w i
      Dungeons -> gameLoop $ handleDungeonScene w i
      DungeonPrepare -> gameLoop $ handleDungeonPrepareScene w i
      FightResultScene -> gameLoop $ handleFightResultScene w i
      HeroInfo -> gameLoop $ handleHeroInfoScene w i
      Fight -> gameLoop w

game :: IO ()
game = do
  gameInit
  rGen <- getStdGen
  gameLoop $ defaultWorld rGen

handleExit :: IO ()
handleExit = do
  clearScreen
  setCursorPosition 0 0
  setSGR [Reset]
  showCursor
  putStrLn "exit game"

-- The threadDelay is used so that the screen does not Blink
sample :: Int -> IO a -> IO (Maybe a)
sample l f
  | l < 0 = fmap Just f
  | l == 0 = return Nothing
  | otherwise = concurrently (timeout l f) (threadDelay l)
                >>= \ (result, _) -> return result
