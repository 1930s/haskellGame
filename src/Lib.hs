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
import Core.DungeonPrepState

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
      Main -> handleMainScene w i
      Dungeons -> handleDungeonScene w i
      DungeonPrepare -> handleDungeonPrepareScene w i
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
          _ -> world

handleHeroInfoScene :: World -> Input -> IO()
handleHeroInfoScene world inp = gameLoop nw
  where nw = case inp of
          M -> world{currentScene = Main}
          _ -> world

handleDungeonPrepareScene :: World -> Input -> IO()
handleDungeonPrepareScene world@World{dungeonPrep = d_prep} inp = gameLoop nw
  where
    newPrep n = d_prep{team = (heros world !! n) : (team d_prep)}
    nw = case inp of
      D -> world{currentScene = Dungeons}
      S -> world{currentScene = Dungeons}
      (Input n) -> world{dungeonPrep = newPrep n}

game :: IO ()
game = do
  gameInit
  rGen <- getStdGen
  gameLoop $ World {
    currentScene = Main,
    dungeonPrep = DungeonPrepState [],
    heros = [Hero {
                name = "hero1",
                maxHP = 10,
                hp = 10,
                atk = 2,
                level = 1,
                curExp = 0,
                expCap = 10
                  }],
    dungeons = [Dungeon { name = "d1" ,
                          enemies = [Enemy {
                                 name = "enemy1",
                                 maxHP = 5,
                                 hp = 5,
                                 atk = 1,
                                 expReward = 5,
                                 moneyReward = 10
                                    }],
                          timeTaken = 10,
                          herosInDungeon = [],
                          countDown = 0,
                          randomGen = rGen
                        }],
    randomGen = rGen
    }

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
