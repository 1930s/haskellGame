{-# LANGUAGE DuplicateRecordFields #-}

module Dungeon where

import Data.List
import Enemy
import Hero

-- for MVP assume only one battle
-- n v n battle
data Dungeon = Dungeon {
  name :: String,
  enemies :: [Enemy],
  timeTaken :: Int,
  herosInDungeon :: [Hero],
  countDown :: Int
  }

data BattleResult = BattleResult {
  money :: Int,
  updatedHero :: [Hero]
  }

updateDungeon :: Int -> Dungeon -> (Dungeon, Maybe BattleResult)
updateDungeon interval d@Dungeon{countDown = cd}
  | cd > 0 = (d {countDown = max 0 $ cd - interval}, Just result)
  | otherwise = (d {countDown = 0}, Nothing)
  where result = calcBattle d


calcBattle :: Dungeon -> BattleResult
calcBattle Dungeon{enemies = es, herosInDungeon = hs} =
  calcBattle_ es BattleResult{money = 0, updatedHero = hs}

calcBattle_ :: [Enemy] -> BattleResult -> BattleResult
calcBattle_ es bs@BattleResult{money = m, updatedHero = hs}
  | enemyAllDead || heroAllDead = bs
  | otherwise = calcBattle_ updatedEs updatedBs
    where
      enemyAllDead = (sum $ fmap (hp::Enemy -> Int) es) == 0
      heroAllDead = (sum $ fmap (hp::Hero-> Int) hs) == 0
  -- use random in below and update money
      updatedEs = es
      updatedBs = bs

instance Show Dungeon where
  show dungeon = intercalate "\n" toDisplay
    where
      show_name = show (name (dungeon :: Dungeon))
      show_num = "number of enemies: " ++ (show $length (enemies dungeon))
      show_time = "time taken to complete: " ++ (show $ timeTaken dungeon)
      toDisplay = ["Dungeon: ", show_name, show_num, show_time, "--"]

