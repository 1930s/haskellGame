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
import Core.BattleResultPage

data Scene = Main
           | HeroInfo
           | Dungeons
           | DungeonPrepare
           | FightResultScene
           | Fight
           deriving (Eq, Show, Ord)

data World = World {
  currentScene :: Scene ,
  heros :: [Hero],
  dungeonsPage :: DungeonsPage,
  battleResultPage :: BattleResultPage,
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
      FightResultScene -> show $ battleResultPage world
      _ -> "World"

sceneAvailableInput :: Map.Map Scene [Input]
sceneAvailableInput = Map.fromList [
  (Main, [D, H, Q]),
  (Dungeons, [M, J, K, Enter]),
  (DungeonPrepare, [D, A, R, S] ++ (fmap Input [1..9])),
  (FightResultScene, [D, M]),
  (HeroInfo, [M])]

isInputUseful :: World -> Input -> Bool
isInputUseful (World {currentScene = scene}) i =
  case availableList of
      Just l -> i `elem` l
      Nothing -> False
  where
    availableList = Map.lookup scene sceneAvailableInput


defaultWorld :: StdGen -> World
defaultWorld rGen = World {
  currentScene = Main,
  dungeonPrep = defaultPrepPage startHeros dungeon1 ,
  heros = startHeros,
  dungeonsPage = DungeonsPage [dungeon1, dungeon2] 0,
  battleResultPage = BattleResultPage BattleResult{money = 0, updatedHero = []},
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
