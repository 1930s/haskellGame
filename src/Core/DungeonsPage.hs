module Core.DungeonsPage where

import Core.Hero
import Core.Dungeon
import Core.Utils
import Data.Maybe
import qualified Brick.Widgets.List as L

data DungeonsPage = DungeonsPage {
  dungeons :: L.List () Dungeon
  }

dungeonsPageTick :: DungeonsPage -> DungeonsPage
dungeonsPageTick dp@DungeonsPage{dungeons = ds} = dp{dungeons = n_ds}
  where n_ds = fmap dungeonTick ds

resetCompletedDungeon :: DungeonsPage -> DungeonsPage
resetCompletedDungeon dPage = dPage{dungeons = n_ds}
  where n_ds = L.listModify (\d -> d{state = NoMission}) o_ds
        o_ds = dungeons dPage

comeBackFromStartMission :: DungeonsPage -> [Hero] -> DungeonsPage
comeBackFromStartMission dPage hs = DungeonsPage{dungeons = n_dungeons}
  where o_dungeons = dungeons dPage
        n_dungeons = L.listModify (startMission hs) o_dungeons

instance Show DungeonsPage where
  show DungeonsPage{dungeons = ds} =
    "Press M to return Main page, J/K to move cursor Down, Up\n" ++
    "Enter to prepare to enter selected Dungeon \n\n"
    -- ++ concat final
    -- where lst = map show ds
    --       final = take c lst ++ dWithCursor ++ drop (c+1) lst
    --       dWithCursor = ['>' : lst !! c]
