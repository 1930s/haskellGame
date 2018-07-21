module Core.DungeonPrepPage (DungeonPrepPage(..)
                            , defaultPrepPage
                            , addOrRemoveHero
                            , addMode
                            , canRemove
                            , removeMode)where

import Core.Hero
import Core.Dungeon
import qualified Core.Utils as U
import Data.Maybe

data Mode = Add | Remove deriving(Show, Eq)

data DungeonPrepPage = DungeonPrepPage{
  benchHeros :: [Hero],
  team :: [Hero],
  mode :: Mode,
  dungeon :: Dungeon
  }

addOrRemoveHero :: Int -> DungeonPrepPage -> DungeonPrepPage
addOrRemoveHero idx dpp@DungeonPrepPage{mode = md, team = tm, benchHeros = allHs}
  | idx < 0 = undefined
  | md == Add = if addValid
                then dpp{team = tm ++ [allHs !! idx], benchHeros = fromJust $ U.removeAt allHs idx}
                else dpp
  | md == Remove = if removeValid
                   then dpp {team = fromJust $ U.removeAt tm idx, benchHeros = allHs ++ [tm!! idx]}
                   else dpp
  | otherwise = dpp
    where addValid = idx < length allHs
          removeValid = idx < length tm

canRemove :: Int -> DungeonPrepPage -> Bool
canRemove n dp = n > 0 && n < maxL
  where maxL = length $ team dp

defaultPrepPage :: [Hero] -> Dungeon -> DungeonPrepPage
defaultPrepPage heros d = DungeonPrepPage{team = [], dungeon = d, mode = Add, benchHeros = heros}

addMode :: DungeonPrepPage -> DungeonPrepPage
addMode d = d{mode = Add}

removeMode :: DungeonPrepPage -> DungeonPrepPage
removeMode d = d{mode = Remove}

instance Show DungeonPrepPage where
  show DungeonPrepPage{team = tm, dungeon = dg, mode = m, benchHeros = bench} =
    "Mode : " ++ show m ++ " Heros\n\n" ++
    show dg ++ "\n" ++
    "Heros to enter: " ++ show tm ++ "\n\n" ++
    "Heros on bench: " ++ show bench
