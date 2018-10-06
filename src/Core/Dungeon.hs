{-# LANGUAGE DuplicateRecordFields #-}

module Core.Dungeon (Dungeon(..)
                    , defaultDungeon
                    , startMission
                    , dungeonTick
                    , BattleResult(..)
                    , DungeonState(..)) where

import Constants
import Core.Enemy
import Core.Hero
import Core.Utils(CursorName(Normal))

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

data DungeonState = NoMission
                  | InProgress
                  | MissionComplete deriving(Eq, Show)

-- for MVP assume only one battle
-- n vs n battle
data Dungeon = Dungeon {
  dName :: String,
  enemies :: [Enemy],
  missionLength :: Int,
  herosInDungeon :: L.List CursorName Hero,
  state :: DungeonState,
  countDown :: Int
  }


dungeonTick :: Dungeon -> Dungeon
dungeonTick d@Dungeon{countDown = 0, state = InProgress} = d{state = MissionComplete}
dungeonTick dungeon = case state dungeon of
                        InProgress -> dungeon{countDown = newCountDown}
                        _ -> dungeon
                      where newCountDown = max 0 $ countDown dungeon - 1

startMission :: L.List CursorName Hero -> Dungeon -> Dungeon
startMission hs dg = dg{state = InProgress, countDown = missionLength dg, herosInDungeon = hs}

defaultDungeon :: String -> [Enemy] -> Dungeon
defaultDungeon n es = Dungeon {
  dName = n ,
  enemies = es,
  missionLength = 10 * inputRate,
  herosInDungeon = L.list Normal (Vec.fromList []) 1,
  state = NoMission,
  countDown = 0
  }

data BattleResult = BattleResult {
  money :: Int,
  updatedHero :: L.List CursorName Hero
  }

