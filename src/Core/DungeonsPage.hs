module Core.DungeonsPage where

import Core.Dungeon
import Core.DungeonPrepPage
import Data.List

data DungeonsPage = DungeonsPage {
  dungeons :: [Dungeon],
  currentSelection :: Int
  }

comeBackFromStartMission :: DungeonPrepPage -> DungeonsPage -> DungeonsPage
comeBackFromStartMission prepPage dPage = DungeonsPage{dungeons = n_dungeons, currentSelection = 0}
  where dg = dungeon prepPage
        o_dungeons = dungeons dPage
        n_dungeons = delete dg o_dungeons ++ [startMission dg]

getSelectedDungeon :: DungeonsPage -> Dungeon
getSelectedDungeon dp = dungeons dp !! currentSelection dp

selectUp :: DungeonsPage -> DungeonsPage
selectUp dp@DungeonsPage{currentSelection = cs} = dp {currentSelection = n_cs}
  where n_cs = max 0 (cs-1)

selectDown :: DungeonsPage -> DungeonsPage
selectDown dp@DungeonsPage{dungeons = ds, currentSelection = cs} = dp {currentSelection = n_cs}
  where n_cs = min (length ds-1) (cs+1)

instance Show DungeonsPage where
  show DungeonsPage{dungeons = ds, currentSelection = c} =
    "Press M to return Main page, J/K to move cursor Down, Up, Enter to prepare to enter selected Dungeon\n"
    ++ concat final
    where lst = map show ds
          final = take c lst ++ dWithCursor ++ drop (c+1) lst
          dWithCursor = ['>' : lst !! c]
