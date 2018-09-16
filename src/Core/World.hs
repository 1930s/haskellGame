{-# LANGUAGE DuplicateRecordFields #-}

module Core.World where

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import System.Random
import Core.Utils(CursorName(..))
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
  wealth :: Int,
  currentScene :: Scene ,
  options :: L.List CursorName String,
  heros :: L.List CursorName Hero,
  dungeonsPage :: DungeonsPage,
  battleResultPage :: BattleResultPage,
  dungeonPrep :: DungeonPrepPage,
  randomGen :: StdGen
  }

defaultWorld :: StdGen -> World
defaultWorld rGen = World {
  wealth = 0,
  options = L.list Normal (Vec.fromList ["Heros", "Dungeons"] ) 1,
  currentScene = Main,
  dungeonPrep = defaultPrepPage (L.list DungeonPrepareBench (Vec.fromList startHeros) 1) dungeon1 ,
  heros = L.list Normal (Vec.fromList startHeros) 1,
  dungeonsPage = DungeonsPage $ L.list Normal (Vec.fromList [dungeon1, dungeon2]) 1,
  battleResultPage = BattleResultPage BattleResult{money = 0,
                                                   updatedHero = L.list Normal (Vec.fromList []) 1},
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
