{-# LANGUAGE DuplicateRecordFields #-}

module Core.Dungeon (Dungeon(..)
                    , defaultDungeon
                    , startMission
                    , dungeonTick
                    , DungeonState(..)) where

import System.Random
import Data.List
import Constants
import Core.Enemy
import Core.Hero

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
dungeonTick dungeon = case state dungeon of
                        InProgress -> dungeon{countDown = newCountDown}
                        _ -> dungeon
                      where newCountDown = max 0 $ countDown dungeon - 1

startMission :: Dungeon -> Dungeon
startMission dg = dg{state = InProgress, countDown = missionLength dg}

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
  }

processInBattleDungeon :: Int -> StdGen -> Dungeon -> (Dungeon, Maybe BattleResult)
processInBattleDungeon interval rGen d@Dungeon{countDown = cd}
  | cd > 0 = (d {countDown = max 0 $ cd - interval}, Nothing)
  | otherwise = (d {countDown = 0, enemies = resetedEnemies, herosInDungeon=[]}, Just result)
  where result = calcBattle d rGen
        resetedEnemies = map resetEnemy $ enemies d
        resetEnemy e@Enemy{maxHP = m_hp} = e{hp = m_hp} :: Enemy

calcBattle :: Dungeon -> StdGen -> BattleResult
calcBattle Dungeon{enemies = es, herosInDungeon = hs} gen =
  calcBattle_ es gen BattleResult{money = 0, updatedHero = hs}

calcBattle_ :: [Enemy] -> StdGen -> BattleResult -> BattleResult
calcBattle_ es rGen bs@BattleResult{money = m, updatedHero = (h:hs)}
  | enemyAllDead || heroAllDead = bs
  | otherwise = calcBattle_ updatedEs newGen updatedBs
    where
      (randomEnemyIndex, newGen) = randomR (0, length es - 1) rGen
      enemyToAtk = es !! randomEnemyIndex
      (newHero, newEnemy, m_reward) = updateHeroAfterAtk $ exchangeAtk h enemyToAtk
      updatedEs = take randomEnemyIndex es ++ [newEnemy] ++ drop (randomEnemyIndex +1) es
      updatedHs = hs ++ [newHero]
      enemyAllDead = (sum $ fmap (hp::Enemy -> Int) updatedEs) == 0
      heroAllDead = (sum $ fmap (hp::Hero-> Int) updatedHs) == 0
      updatedBs = bs{money = m + m_reward, updatedHero = updatedHs}

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
  show dungeon = intercalate "\n" toDisplay ++ "\n"
    where
      progress = countDown dungeon `div` inputRate
      show_state = show $ state dungeon
      show_progress = case state dungeon of
                        InProgress -> "Progress: " ++ (take progress $ repeat '#')
                        _ -> ""
      show_name = show (name (dungeon :: Dungeon))
      show_num = "number of enemies: " ++ (show $length (enemies dungeon))
      show_time = "time taken to complete: " ++ (show $ missionLength dungeon)
      toDisplay = ["Dungeon: ", show_name, show_num, show_time, show_state, show_progress, "--"]
