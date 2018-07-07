{-# LANGUAGE DuplicateRecordFields #-}

module World where

import Coord
import Data.List
import qualified Data.Map.Strict as Map
import Input

data Scene = Main
           | HeroInfo
           | Dungeons
           | Fight
           deriving (Eq, Show, Ord)

data Hero = Hero {
  name :: String,
  hp :: Int,
  atk :: Int
  }

data Enemy = Enemy {
  name :: String,
  hp :: Int,
  atk :: Int,
  expReward :: Int,
  moneyReward :: Int
  } deriving (Show)

data Dungeon = Dungeon {
  name :: String,
  enemies :: [Enemy]
  }

data World = World {
  wHero :: Coord ,
  currentScene :: Scene ,
  heros :: [Hero],
  dungeons :: [Dungeon]
  }

instance Show Hero where
  show (Hero {name = n, hp = h, atk = a}) = intercalate "\n" toDisplay
    where show_name = "Name: " ++ show n
          show_hp = "HP: " ++ show h
          show_atk = "ATK: " ++ show a
          toDisplay = ["Hero:", show_name, show_hp, show_atk, "--"]

instance Show Dungeon where
  show dungeon = intercalate "\n" toDisplay
    where
      show_name = show (name (dungeon :: Dungeon))
      show_num = "number of enemies: " ++ (show $length (enemies dungeon))
      toDisplay = ["Dungeon: ", show_name, show_num, "--"]

instance Show World where
  show World {currentScene = scene, heros = allHero, dungeons = allDungeon} =
    case scene of
      HeroInfo -> show allHero
      Main -> "press h to goto all heros \n" ++ "Number of heros: " ++ (show $ length allHero)
              ++ "press g to all dungones \n"
      Dungeons -> show allDungeon
      _ -> "World"


sceneAvailableInput :: Map.Map Scene [Input]
sceneAvailableInput = Map.fromList [(Main, ['g'])]
