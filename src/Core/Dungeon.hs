{-# LANGUAGE DuplicateRecordFields #-}

module Core.Dungeon (Dungeon(..)
                    , defaultDungeon
                    , startMission
                    , dungeonTick
                    , calcBattle
                    , BattleResult(..)
                    , DungeonState(..)) where

import System.Random
import Data.Maybe
import Constants
import Core.Enemy
import Core.Hero
import Core.Utils(replaceAt, CursorName(Normal))

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

data DungeonState = NoMission
                  | InProgress
                  | MissionComplete deriving(Eq, Show)

-- for MVP assume only one battle
-- n vs n battle
data Dungeon = Dungeon {
  name :: String,
  enemies :: [Enemy],
  missionLength :: Int,
  herosInDungeon :: L.List CursorName Hero,
  state :: DungeonState,
  countDown :: Int
  }

instance Eq Dungeon where
  d == d2 = name (d :: Dungeon ) == name (d2 :: Dungeon )

dungeonTick :: Dungeon -> Dungeon
dungeonTick d@Dungeon{countDown = 0, state = InProgress} = d{state = MissionComplete}
dungeonTick dungeon = case state dungeon of
                        InProgress -> dungeon{countDown = newCountDown}
                        _ -> dungeon
                      where newCountDown = max 0 $ countDown dungeon - 1

startMission :: L.List CursorName Hero -> Dungeon -> Dungeon
startMission hs dg = dg{state = InProgress, countDown = missionLength dg, herosInDungeon = hs}

defaultDungeon :: String -> [Enemy] -> Dungeon
defaultDungeon n es = Dungeon {
  name = n ,
  enemies = es,
  missionLength = 10 * inputRate,
  herosInDungeon = L.list Normal (Vec.fromList []) 1,
  state = NoMission,
  countDown = 0
  }

data BattleResult = BattleResult {
  money :: Int,
  updatedHero :: L.List CursorName Hero
  }

calcBattle :: Dungeon -> StdGen -> BattleResult
calcBattle Dungeon{enemies = es, herosInDungeon = hs} gen =
  calcBattle_ es gen BattleResult{money = 0, updatedHero = hs}

calcBattle_ :: [Enemy] -> StdGen -> BattleResult -> BattleResult
calcBattle_ es rGen bs@BattleResult{money = m, updatedHero = hs}
  | enemyAllDead || heroAllDead = bs
  | otherwise = calcBattle_ updatedEs newGen updatedBs
    where
      aliveEnemies = filter (\e -> hp (e::Enemy) > 0) es
      aliveHeros = Vec.filter (\h -> hp (h::Hero) > 0) $ L.listElements hs
      actionHero = Vec.head aliveHeros
      (randomEnemyIndex, newGen) = randomR (0, length aliveEnemies - 1) rGen
      enemyToAtk = aliveEnemies !! randomEnemyIndex
      (newHero, newEnemy, m_reward) = updateAfterAtk $ exchangeAtk actionHero enemyToAtk
      updatedEs = fromJust $ replaceAt aliveEnemies newEnemy randomEnemyIndex
      updatedHs = Vec.tail aliveHeros Vec.++ Vec.fromList [newHero]
      enemyAllDead = length aliveEnemies == 0
      heroAllDead = length aliveHeros == 0
      updatedBs = bs{money = m + m_reward, updatedHero = L.list Normal updatedHs 1}

updateAfterAtk :: (Hero, Enemy) -> (Hero, Enemy, Int)
updateAfterAtk (h@Hero{expCap = o_cap, curExp = o_exp, level = o_l}, e)
  | hp (e::Enemy) <= 0 = (h{level = n_l, curExp = n_exp, expCap = n_cap}, e, m_reward)
  | otherwise = (h, e, 0)
    where n_l = if levelUp then o_l+1 else o_l
          n_exp = o_exp + e_reward
          n_cap = if levelUp then o_cap * 3 else o_cap
          levelUp = o_exp + e_reward >= o_cap
          e_reward = expReward e
          m_reward = moneyReward e

-- attack each other at the same time
exchangeAtk :: Hero -> Enemy -> (Hero, Enemy)
exchangeAtk h@Hero{atk = h_atk, hp = h_hp} e@Enemy{atk = e_atk, hp = e_hp} =
  (nh, ne)
  where nh = h{hp = max 0 h_hp - e_atk} :: Hero
        ne = e{hp = max 0 e_hp - h_atk} :: Enemy
