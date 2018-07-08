{-# LANGUAGE DuplicateRecordFields #-}

module Core.Dungeon where

import System.Random
import Data.List
import Core.Enemy
import Core.Hero

-- for MVP assume only one battle
-- n v n battle
data Dungeon = Dungeon {
  name :: String,
  enemies :: [Enemy],
  timeTaken :: Int,
  herosInDungeon :: [Hero],
  countDown :: Int,
  randomGen :: StdGen
  }

data BattleResult = BattleResult {
  money :: Int,
  updatedHero :: [Hero],
  randomGen :: StdGen
  }

updateDungeon :: Int -> Dungeon -> (Dungeon, Maybe BattleResult)
updateDungeon interval d@Dungeon{countDown = cd}
  | cd > 0 = (d {countDown = max 0 $ cd - interval}, Nothing)
  | otherwise = (d {countDown = 0}, Just result)
  where result = calcBattle d

calcBattle :: Dungeon -> BattleResult
calcBattle Dungeon{enemies = es, herosInDungeon = hs, randomGen = rGen} =
  calcBattle_ es BattleResult{money = 0, updatedHero = hs, randomGen = rGen}

calcBattle_ :: [Enemy] -> BattleResult -> BattleResult
calcBattle_ es bs@BattleResult{money = m, updatedHero = (h:hs), randomGen = rGen}
  | enemyAllDead || heroAllDead = bs
  | otherwise = calcBattle_ updatedEs updatedBs
    where
      (randomEnemyIndex, newGen) = randomR (0, length es - 1) rGen
      enemyToAtk = es !! randomEnemyIndex
      (newHero, newEnemy, m_reward) = updateHeroAfterAtk $ exchangeAtk h enemyToAtk
      updatedEs = take randomEnemyIndex es ++ [newEnemy] ++ drop (randomEnemyIndex +1) es
      updatedHs = hs ++ [newHero]
      enemyAllDead = (sum $ fmap (hp::Enemy -> Int) updatedEs) == 0
      heroAllDead = (sum $ fmap (hp::Hero-> Int) updatedHs) == 0
      updatedBs = bs{money = m + m_reward, updatedHero = updatedHs, randomGen = newGen}

updateHeroAfterAtk :: (Hero, Enemy) -> (Hero, Enemy, Int)
updateHeroAfterAtk (h@Hero{expCap = o_cap, curExp = o_exp, level = o_l}, e)
  | hp (e::Enemy) <= 0 = (h{level = n_l, curExp = n_exp, expCap = n_cap}, e, m_reward)
  | otherwise = (h, e, 0)
    where n_l = if levelUp then o_l+1 else o_l
          n_exp = if levelUp then o_exp + e_reward else o_exp + e_reward - o_cap
          n_cap = if levelUp then o_cap * 2 else o_cap
          levelUp = o_exp + e_reward > o_cap
          e_reward = expReward e
          m_reward = moneyReward e

exchangeAtk :: Hero -> Enemy -> (Hero, Enemy)
exchangeAtk h@Hero{atk = h_atk, hp = h_hp} e@Enemy{atk = e_atk, hp = e_hp} =
  (nh, ne)
  where nh = h{hp = max 0 h_hp - e_atk} :: Hero
        ne = e{hp = max 0 e_hp - h_atk} :: Enemy

instance Show Dungeon where
  show dungeon = intercalate "\n" toDisplay
    where
      show_name = show (name (dungeon :: Dungeon))
      show_num = "number of enemies: " ++ (show $length (enemies dungeon))
      show_time = "time taken to complete: " ++ (show $ timeTaken dungeon)
      toDisplay = ["Dungeon: ", show_name, show_num, show_time, "--"]

