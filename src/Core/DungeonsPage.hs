module Core.DungeonsPage where

import Core.Dungeon

data DungeonsPage = DungeonsPage {
  dungeons :: [Dungeon],
  currentSelection :: Int
  }

getSelectedDungeon :: DungeonsPage -> Dungeon
getSelectedDungeon dp = dungeons dp !! currentSelection dp

selectUp :: DungeonsPage -> DungeonsPage
selectUp dp@DungeonsPage{currentSelection = cs} = dp {currentSelection = n_cs}
  where n_cs = max 0 (cs-1)

selectDown :: DungeonsPage -> DungeonsPage
selectDown dp@DungeonsPage{dungeons = ds, currentSelection = cs} = dp {currentSelection = n_cs}
  where n_cs = min (length ds-1) (cs+1)

instance Show DungeonsPage where
  show DungeonsPage{dungeons = ds, currentSelection = c} = concat final
    where lst = map show ds
          final = take c lst ++ dWithCursor ++ drop (c+1) lst
          dWithCursor = ['>' : lst !! c]
