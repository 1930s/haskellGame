{-# LANGUAGE DuplicateRecordFields #-}

module World where

import Coord
import Data.List
import qualified Data.Map.Strict as Map
import Input
import Hero
import Enemy
import Dungeon

data Scene = Main
           | HeroInfo
           | Dungeons
           | Fight
           deriving (Eq, Show, Ord)

data World = World {
  wHero :: Coord ,
  currentScene :: Scene ,
  heros :: [Hero],
  dungeons :: [Dungeon]
  }

instance Show World where
  show World {currentScene = scene, heros = allHero, dungeons = allDungeon} =
    case scene of
      HeroInfo -> concat $ map show allHero
      Main -> "press h to goto all heros \n" ++ "Number of heros: " ++ (show $ length allHero) ++ "\n"
              ++ "press g to all dungones \n"
      Dungeons -> concat $ map show allDungeon
      _ -> "World"

sceneAvailableInput :: Map.Map Scene [Input]
sceneAvailableInput = Map.fromList [
  (Main, [D, H, Q]),
  (Dungeons, [M]),
  (HeroInfo, [M])]

isInputUseful :: World -> Input -> Bool
isInputUseful (World {currentScene = scene}) i =
  case availableList of
      Just l -> i `elem` l
      Nothing -> False
  where
    availableList = Map.lookup scene sceneAvailableInput
