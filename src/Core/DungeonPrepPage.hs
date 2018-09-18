module Core.DungeonPrepPage (DungeonPrepPage(..)
                            , defaultPrepPage
                            , addOrRemoveHero
                            , addMode
                            , removeMode
                            , moveUpDownselection
                            , Mode(..)
                            )where

import Data.Vector as Vec (fromList)
import Core.Hero
import Core.Dungeon
import Core.Utils (CursorName(..))
import Data.Maybe
import qualified Brick.Widgets.List as L
import Input

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
  } = let
  selected = L.listSelectedElement benchHs
  in case selected of
       Just (heroRemoveIdx, heroToAdd) -> dpp{team = newTeam, benchHeros = newBenchHeros}
         where newTeam = L.listInsert (length tm) heroToAdd tm
               newBenchHeros = L.listRemove heroRemoveIdx benchHs
       Nothing -> dpp

removeHeroToTeam :: DungeonPrepPage -> DungeonPrepPage
removeHeroToTeam dpp@DungeonPrepPage{
  team = tm,
  benchHeros = benchHs
  } =  let
  selected = L.listSelectedElement tm
  in case selected of
       Just (heroRemoveIdx, heroToRemove) -> dpp{team = newTeam, benchHeros = newBenchHeros}
         where newTeam = L.listRemove heroRemoveIdx tm
               newBenchHeros = L.listInsert (length benchHs) heroToRemove benchHs
       Nothing -> dpp

addOrRemoveHero :: DungeonPrepPage -> DungeonPrepPage
addOrRemoveHero dpp@DungeonPrepPage{mode = md} =
  case md of
    Add -> addHeroToTeam dpp
    Remove -> removeHeroToTeam dpp

moveUpDownselection :: DungeonPrepPage -> Input -> DungeonPrepPage
moveUpDownselection dpp@DungeonPrepPage{
  team = tm,
  benchHeros = benchHs,
  mode = md
  } inp = dpp{team = n_tm, benchHeros = n_hs}
  where move = case inp of
          KeyUp -> L.listMoveUp
          KeyDown -> L.listMoveDown
        n_hs = case md of
          Add -> move benchHs
          Remove -> benchHs
        n_tm = case md of
          Add -> tm
          Remove -> move tm




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
