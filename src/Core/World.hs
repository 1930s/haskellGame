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
import Core.BattlePage
import Core.Equipment
import Core.InventoryPage

data Scene = Main
           | HeroInfoScene
           | DungeonSelectionScene
           | DungeonPrepareScene
           | FightResultScene
           | FightScene
           | InventoryScene
           deriving (Eq, Show, Ord)

data World = World {
  wealth :: Int,
  currentScene :: Scene ,
  options :: L.List CursorName Scene,
  heros :: L.List CursorName Hero,
  inventory :: L.List CursorName Equipment,
  inventoryPage :: InventoryPage,
  dungeonsPage :: DungeonsPage,
  battleResultPage :: BattleResultPage,
  dungeonPrep :: DungeonPrepPage,
  battlePage :: Maybe BattlePage,
  randomGen :: StdGen
  }

defaultWorld :: StdGen -> World
defaultWorld rGen = World {
  wealth = 0,
  options = L.list Normal (Vec.fromList [HeroInfoScene, InventoryScene, DungeonSelectionScene] ) 1,
  inventory = defaultInvs,
  inventoryPage = InventoryPage{allEquipments = defaultInvs},
  currentScene = Main,
  dungeonPrep = defaultPrepPage (L.list DungeonPrepareBench (Vec.fromList startHeros) 1) dungeon1 ,
  heros = L.list Normal (Vec.fromList startHeros) 1,
  dungeonsPage = DungeonsPage $ L.list Normal (Vec.fromList [dungeon1, dungeon2]) 1,
  battleResultPage = BattleResultPage BattleResult{money = 0,
                                                   equipmentDrops = [],
                                                   updatedHero = L.list Normal (Vec.fromList []) 1},
  battlePage = Nothing,
  randomGen = rGen
  }
  where e1 = defaultEnemy "enemy1"
        e2 = defaultEnemy "enemy2"
        e3 = defaultEnemy "enemy3"
        e4 = defaultEnemy "enemy4"
        dungeon1 = defaultDungeon "dungeon1" [e1,e2]
        dungeon2 = defaultDungeon "dungeon2" [e3,e4]
        defaultInvs = L.list Normal (Vec.fromList [] ) 1
        startHeros = [
          defaultHero "hero 1",
          defaultHero "hero 2",
          defaultHero "hero 3"
          ]
