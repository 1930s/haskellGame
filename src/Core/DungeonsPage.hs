module Core.DungeonsPage where

import Core.Hero
import Core.Dungeon
import Core.DungeonPrepPage
import Core.Utils
import Data.List
import Data.Maybe

data DungeonsPage = DungeonsPage {
  dungeons :: [Dungeon],
  currentSelection :: Int
  }

dungeonsPageTick :: DungeonsPage -> DungeonsPage
dungeonsPageTick dp@DungeonsPage{dungeons = ds} = dp{dungeons = n_ds}
  where n_ds = fmap dungeonTick ds

resetCompletedDungeon :: DungeonsPage -> DungeonsPage
resetCompletedDungeon dPage = dPage{dungeons = n_ds}
  where n_ds = fromJust $ modifyAt o_ds (\d -> d{state = NoMission}) idx
        o_ds = dungeons dPage
        idx = currentSelection dPage

comeBackFromStartMission :: DungeonsPage -> [Hero] -> DungeonsPage
comeBackFromStartMission dPage hs = DungeonsPage{dungeons = n_dungeons, currentSelection = 0}
  where o_dungeons = dungeons dPage
        n_dungeons = fromJust $ modifyAt o_dungeons (startMission hs) idx
        idx = currentSelection dPage

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
