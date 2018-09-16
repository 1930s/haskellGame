module Core.DungeonPrepPage (DungeonPrepPage(..)
                            , defaultPrepPage
                            , addOrRemoveHero
                            , addMode
                            , removeMode)where

import Data.Vector as Vec (fromList)
import Core.Hero
import Core.Dungeon
import Core.Utils (CursorName(..))
import Data.Maybe
import qualified Brick.Widgets.List as L

data Mode = Add | Remove deriving(Show, Eq)

data DungeonPrepPage = DungeonPrepPage{
  benchHeros :: L.List CursorName Hero,
  team :: L.List CursorName Hero,
  mode :: Mode,
  dungeon :: Dungeon
  }

addHeroToTeam :: DungeonPrepPage -> DungeonPrepPage
addHeroToTeam dpp@DungeonPrepPage{
  team = tm,
  benchHeros = benchHs
  } = dpp{team = newTeam, benchHeros = newBenchHeros}
  where newTeam = L.listInsert (length tm) heroToAdd tm
        (heroRemoveIdx, heroToAdd) = fromJust $ L.listSelectedElement benchHs
        newBenchHeros = L.listRemove heroRemoveIdx benchHs

removeHeroToTeam :: DungeonPrepPage -> DungeonPrepPage
removeHeroToTeam dpp@DungeonPrepPage{
  team = tm,
  benchHeros = benchHs
  } = dpp{team = newTeam, benchHeros = newBenchHeros}
  where newTeam = L.listRemove heroRemoveIdx tm
        (heroRemoveIdx, heroToRemove) = fromJust $ L.listSelectedElement tm
        newBenchHeros = L.listInsert (length benchHs) heroToRemove benchHs

addOrRemoveHero :: DungeonPrepPage -> DungeonPrepPage
addOrRemoveHero dpp@DungeonPrepPage{mode = md} =
  case md of
    Add -> addHeroToTeam dpp
    Remove -> removeHeroToTeam dpp

defaultPrepPage :: L.List CursorName Hero -> Dungeon -> DungeonPrepPage
defaultPrepPage heros d = DungeonPrepPage{
  team = L.list DungeonPrepareTeam (Vec.fromList []) 1,
  dungeon = d,
  mode = Add,
  benchHeros = heros
  }

addMode :: DungeonPrepPage -> DungeonPrepPage
addMode d = d{mode = Add}

removeMode :: DungeonPrepPage -> DungeonPrepPage
removeMode d = d{mode = Remove}

-- instance Show DungeonPrepPage where
--   show DungeonPrepPage{team = tm, dungeon = dg, mode = m, benchHeros = bench} =
--     "Press D to return DungeonsPage \n" ++
--     "A for Add hero mode, R for Remove hero mode, S to start mission\n" ++
--     "Mode : " ++ show m ++ " Heros\n\n" ++
--     show dg ++ "\n" ++
--     "Heros to enter: " ++ show tm ++ "\n\n" ++
--     "Heros on bench: " ++ show bench
