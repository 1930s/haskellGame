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
import Core.Hero
import Core.Enemy
import Core.Dungeon
import Core.DungeonsPage
import Core.DungeonPrepPage
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
gameLoop world = do
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
      HeroInfo -> gameLoop $ handleHeroInfoScene w i
      Fight -> gameLoop w

game :: IO ()
game = do
  gameInit
  rGen <- getStdGen
  let (_, gen2) = (random rGen) :: (Integer, StdGen)
  gameLoop $ World {
    currentScene = Main,
    dungeonPrep = defaultPrepPage startHeros dungeon1 ,
    heros = startHeros,
    dungeonsPage = DungeonsPage [dungeon1, dungeon2] 0,
    randomGen = rGen
    }
    where e1 = defaultEnemy "enemy1"
          e2 = defaultEnemy "enemy2"
          e3 = defaultEnemy "enemy3"
          e4 = defaultEnemy "enemy4"
          dungeon1 = defaultDungeon "dungeon1" [e1,e2]
          dungeon2 = defaultDungeon "dungeon2" [e3,e4]
          startHeros = [
            Hero {
                name = "hero1",
                maxHP = 10,
                hp = 10,
                atk = 2,
                level = 1,
                curExp = 0,
                expCap = 10
                }]

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
