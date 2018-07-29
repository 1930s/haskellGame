{-# LANGUAGE DuplicateRecordFields #-}

module Core.Dungeon (Dungeon(..)
                    , defaultDungeon
                    , startMission
                    , dungeonTick
                    , calcBattle
                    , BattleResult(..)
                    , DungeonState(..)) where

import System.Random
import Data.List
import Data.Maybe
import Constants
import Core.Enemy
import Core.Hero
import Core.Utils(replaceAt)

data DungeonState = NoMission
                  | InProgress
                  | MissionComplete deriving(Eq, Show)

-- for MVP assume only one battle
-- n vs n battle
data Dungeon = Dungeon {
  name :: String,
  enemies :: [Enemy],
  missionLength :: Int,
  herosInDungeon :: [Hero],
  state :: DungeonState,
  countDown :: Int
  } deriving (Eq)

dungeonTick :: Dungeon -> Dungeon
dungeonTick d@Dungeon{countDown = 0, state = InProgress} = d{state = MissionComplete}
dungeonTick dungeon = case state dungeon of
                        InProgress -> dungeon{countDown = newCountDown}
                        _ -> dungeon
                      where newCountDown = max 0 $ countDown dungeon - 1

startMission :: [Hero] -> Dungeon -> Dungeon
startMission hs dg = dg{state = InProgress, countDown = missionLength dg, herosInDungeon = hs}

defaultDungeon :: String -> [Enemy] -> Dungeon
defaultDungeon n es = Dungeon {
  name = n ,
  enemies = es,
  missionLength = 10 * inputRate,
  herosInDungeon = [],
  state = NoMission,
  countDown = 0
  }

data BattleResult = BattleResult {
  money :: Int,
  updatedHero :: [Hero]
  } deriving (Eq, Show, Ord)

calcBattle :: Dungeon -> StdGen -> BattleResult
calcBattle Dungeon{enemies = es, herosInDungeon = hs} gen =
  calcBattle_ es gen BattleResult{money = 0, updatedHero = hs}

calcBattle_ :: [Enemy] -> StdGen -> BattleResult -> BattleResult
calcBattle_ es rGen bs@BattleResult{money = m, updatedHero = hs}
  | enemyAllDead || heroAllDead = bs
  | otherwise = calcBattle_ updatedEs newGen updatedBs
    where
      aliveEnemies = filter (\e -> hp (e::Enemy) > 0) es
      aliveHeros = filter (\h -> hp (h::Hero) > 0) hs
      actionHero = head aliveHeros
      (randomEnemyIndex, newGen) = randomR (0, length aliveEnemies - 1) rGen
      enemyToAtk = aliveEnemies !! randomEnemyIndex
      (newHero, newEnemy, m_reward) = updateAfterAtk $ exchangeAtk actionHero enemyToAtk
      updatedEs = fromJust $ replaceAt es newEnemy randomEnemyIndex
      updatedHs = tail hs ++ [newHero]
      enemyAllDead = length aliveEnemies == 0
      heroAllDead = length aliveHeros == 0
      updatedBs = bs{money = m + m_reward, updatedHero = updatedHs}

updateAfterAtk :: (Hero, Enemy) -> (Hero, Enemy, Int)
updateAfterAtk (h@Hero{expCap = o_cap, curExp = o_exp, level = o_l}, e)
  | hp (e::Enemy) <= 0 = (h{level = n_l, curExp = n_exp, expCap = n_cap}, e, m_reward)
  | otherwise = (h, e, 0)
    where n_l = if levelUp then o_l+1 else o_l
          n_exp = if levelUp then o_exp + e_reward else o_exp + e_reward - o_cap
          n_cap = if levelUp then o_cap * 2 else o_cap
          levelUp = o_exp + e_reward > o_cap
          e_reward = expReward e
          m_reward = moneyReward e

-- attack each other at the same time
exchangeAtk :: Hero -> Enemy -> (Hero, Enemy)
exchangeAtk h@Hero{atk = h_atk, hp = h_hp} e@Enemy{atk = e_atk, hp = e_hp} =
  (nh, ne)
  where nh = h{hp = max 0 h_hp - e_atk} :: Hero
        ne = e{hp = max 0 e_hp - h_atk} :: Enemy

instance Show Dungeon where
  show dungeon = intercalate "\n" toDisplay ++ "\n"
    where
      progress = countDown dungeon `div` inputRate
      show_state = show $ state dungeon
      show_progress = case state dungeon of
                        InProgress -> "Progress: " ++ (take progress $ repeat '#')
                        MissionComplete -> "Completed!"
                        _ -> ""
      show_name = show (name (dungeon :: Dungeon))
      show_num = "number of enemies: " ++ (show $length (enemies dungeon))
      show_time = "time taken to complete: " ++ (show $ missionLength dungeon)
      toDisplay = ["Dungeon: ", show_name, show_num, show_time, show_state, show_progress, "--"]
