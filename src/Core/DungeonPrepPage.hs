module Core.DungeonPrepPage where

import Core.Hero
import Core.Dungeon

data DungeonPrepPage = DungeonPrepPage{
  team :: [Hero],
  dungeon :: Dungeon
  }

instance Show DungeonPrepPage where
  show DungeonPrepPage{team = tm, dungeon = dg} =
    show dg ++ "\n" ++ show tm
