{-# LANGUAGE DuplicateRecordFields #-}

module Core.World where

import System.Random
import Data.List
import qualified Data.Map.Strict as Map
import Input
import Core.Hero
import Core.Enemy
import Core.Dungeon
import Core.DungeonsPage
import Core.DungeonPrepPage

data Scene = Main
           | HeroInfo
           | Dungeons
           | DungeonPrepare
           | Fight
           deriving (Eq, Show, Ord)

data World = World {
  currentScene :: Scene ,
  heros :: [Hero],
  dungeonsPage :: DungeonsPage,
  dungeonPrep :: DungeonPrepPage,
  randomGen :: StdGen
  }

instance Show World where
  show world@World {currentScene = scene, heros = allHero, dungeonsPage = d_page} =
    case scene of
      HeroInfo -> concat $ map show allHero
      Main -> "press h to goto all heros \n" ++ "Number of heros: " ++ (show $ length allHero) ++ "\n"
              ++ "press d to all dungones \n"
      Dungeons -> show d_page
      DungeonPrepare -> show $ dungeonPrep world
      _ -> "World"

sceneAvailableInput :: Map.Map Scene [Input]
sceneAvailableInput = Map.fromList [
  (Main, [D, H, Q]),
  (Dungeons, [M, J, K, Enter]),
  (DungeonPrepare, [D, A, R] ++ (fmap Input [1..9])),
  (HeroInfo, [M])]

isInputUseful :: World -> Input -> Bool
isInputUseful (World {currentScene = scene}) i =
  case availableList of
      Just l -> i `elem` l
      Nothing -> False
  where
    availableList = Map.lookup scene sceneAvailableInput
