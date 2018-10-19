module Core.DungeonsPage where

import Core.Hero
import Core.Dungeon
import Core.Utils
import qualified Brick.Widgets.List as L

data DungeonsPage = DungeonsPage {
  dungeons :: L.List CursorName Dungeon
  }

dungeonsPageTick :: DungeonsPage -> DungeonsPage
dungeonsPageTick dp@DungeonsPage{dungeons = ds} = dp{dungeons = n_ds}
  where n_ds = fmap dungeonTick ds

resetCompletedDungeon :: DungeonsPage -> DungeonsPage
resetCompletedDungeon dPage = dPage{dungeons = n_ds}
  where n_ds = L.listModify (\d -> d{state = NoMission}) o_ds
        o_ds = dungeons dPage

comeBackFromStartMission :: DungeonsPage -> L.List CursorName Hero -> DungeonsPage
comeBackFromStartMission dPage hs = DungeonsPage{dungeons = n_dungeons}
  where o_dungeons = dungeons dPage
        n_dungeons = L.listModify (startMission hs) o_dungeons

selectUp :: DungeonsPage -> DungeonsPage
selectUp d@DungeonsPage{dungeons = ds} = d{dungeons = L.listMoveUp ds}

selectDown :: DungeonsPage -> DungeonsPage
selectDown d@DungeonsPage{dungeons = ds} = d{dungeons = L.listMoveDown ds}
