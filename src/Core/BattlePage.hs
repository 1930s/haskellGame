module Core.BattlePage(
  BattlePage(..),
  initialiseBattlePage,
  handleSelectDown,
  handleSelectUp,
  handleConfirmAction,
  handleGameTick,
  isFightOver,
  generateBattleResult
  ) where

import System.Random
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Core.Utils as U
import Core.Hero
import Core.Enemy
import Core.Utils
import Core.BattleResultPage
import qualified Core.Dungeon as DG

import Data.List
import Data.Maybe
import Control.Lens

data BattleState = HeroTurn
                 | EnemyAttacking
                 deriving(Show)

data BattlePage = BattlePage{
  heros :: L.List U.CursorName Hero,
  deadHeros :: L.List U.CursorName Hero,
  enemies :: L.List U.CursorName Enemy,
  deadEnemies :: L.List U.CursorName Enemy,
  attackSequence :: [String],
  currentAttacker :: String,
  state :: BattleState,
  totalReward :: Int,
  attackFrame :: Int,
  countDown :: Int,
  randomGen :: StdGen
  }

initialiseBattlePage :: L.List U.CursorName Hero
                     -> L.List U.CursorName Enemy
                     -> StdGen
                     -> Int
                     -> BattlePage
initialiseBattlePage hs es gen frame = selectHero curAttacker $ BattlePage{
  heros = hs,
  deadHeros = emptyList,
  enemies = es,
  deadEnemies = emptyList,
  attackSequence = atkSeq,
  currentAttacker = curAttacker,
  state = HeroTurn,
  totalReward = 0,
  attackFrame = frame,
  countDown = 0,
  randomGen = gen}
  where curAttacker = head atkSeq
        emptyList = L.list U.Normal (Vec.fromList []) 1
        atkSeq = Vec.toList
                 $ (fmap (\h -> h^.name) $ L.listElements hs)
                 Vec.++
                 (fmap (\e -> (e^.eName)) $ L.listElements es)

-- enemy in sequence attack a random enemy
enemyAttackRandomHero :: BattlePage -> BattlePage
enemyAttackRandomHero bp@BattlePage{state = HeroTurn} = bp
enemyAttackRandomHero bp@BattlePage{
  state = EnemyAttacking,
  heros = hs,
  randomGen = generator,
  currentAttacker = curAttacker
  } = moveToNextAttacker $ processFunc bp{heros = updatedHeros
        , randomGen = newGen}
  where updatedHeros = listReplaceAt heroUnderAttackIdx updatedHero hs
        processFunc = case isHeroAlive updatedHero of
          True -> id
          False -> handleHeroDeath updatedHero
        enemy = getEnemyFromAttackSequence curAttacker bp
        updatedHero = heroTakeAttack (enemy^.eAtk) heroUnderAttack
        heroUnderAttack = L.listElements hs Vec.! heroUnderAttackIdx
        (heroUnderAttackIdx, newGen) = randomR (0, length hs -1) generator

-- enemy take dmg
-- hero get reward if enemyDead
-- change attackSequence
performHeroAttack :: Int -> BattlePage -> BattlePage
performHeroAttack enemyIdx bp@BattlePage{
  heros = hs,
  enemies = es
  } = moveToNextAttacker $ processFunc $ bp{ enemies = updatedEnemies
        , heros = updatedHeros}
  where updatedEnemies = listReplaceAt enemyIdx enemyAfterAttack es
        updatedHeros = L.listModify (\_ -> heroAfterAttack) hs
        (_, attackHero) = fromJust $ L.listSelectedElement hs
        enemyUnderAttack = (L.listElements es) Vec.! enemyIdx
        enemyAfterAttack = enemyTakeAttack (attackHero^.totalAtk) enemyUnderAttack
        heroAfterAttack = heroReceiveExp expRwd attackHero
        (processFunc, expRwd) = case (isAlive enemyAfterAttack) of
          False -> (handleEnemyDeath enemyAfterAttack, enemyAfterAttack^.eExpReward )
          _ -> (id, 0)

handleEnemyDeath :: Enemy -> BattlePage -> BattlePage
handleEnemyDeath enemy bp@BattlePage{
  attackSequence = atkSeq,
  totalReward = totalRwd,
  deadEnemies = des,
  enemies = es
  } = bp{
  attackSequence = updatedAttackSeq,
  totalReward = totalRwd + moneyRwd,
  deadEnemies = L.listInsert 0 enemy des,
  enemies = L.listRemove deadEnemyIdx es
  }
  where updatedAttackSeq = atkSeq \\ [enemy^.eName]
        moneyRwd = enemy^.eMoneyReward
        deadEnemyIdx = fromJust $ Vec.findIndex matchEnemyByName $ L.listElements es
        matchEnemyByName e = e^.eName == enemy^.eName


handleHeroDeath :: Hero -> BattlePage -> BattlePage
handleHeroDeath hero bp@BattlePage{
  heros = hs,
  deadHeros = dhs,
  attackSequence = atkSeq
  } = bp{
  attackSequence = updatedAttackSeq,
  heros = L.listRemove deadHeroIdx hs,
  deadHeros = L.listInsert 0 hero dhs
  }
  where updatedAttackSeq = atkSeq \\ [hero^.name ]
        deadHeroIdx = fromJust $ Vec.findIndex matchHeroByName $ L.listElements hs
        matchHeroByName h = hero == h


selectHero :: String -> BattlePage -> BattlePage
selectHero nm bp = bp{heros = L.listMoveTo idx $ heros bp}
  where idx = fromJust $ Vec.findIndex (\h -> h^.name  == nm)
              $ L.listElements $ heros bp

selectEnemy :: String -> BattlePage -> BattlePage
selectEnemy nm bp = bp{enemies = L.listMoveTo idx $ enemies bp}
  where idx = fromJust $ Vec.findIndex (\e -> e^.eName == nm)
              $ L.listElements $ enemies bp

getEnemyFromAttackSequence :: String -> BattlePage -> Enemy
getEnemyFromAttackSequence s bp = fromJust
                                  $ Vec.find nameMatch
                                  $ L.listElements $ enemies bp
  where nameMatch e = e^.eName  == s

isEnemyAttackNext :: BattlePage -> Bool
isEnemyAttackNext bp =
  case res of
    Nothing -> False
    Just _ -> True
  where res = Vec.findIndex matchFunc $ L.listElements $ enemies bp
        matchFunc e = e^.eName == currentAttacker bp

isFightOver :: BattlePage -> Bool
isFightOver BattlePage{
  heros = hs,
  enemies = es
  } =
  0 == (Vec.length $ L.listElements hs) ||
  0 == (Vec.length $ L.listElements es)

moveToNextAttacker :: BattlePage -> BattlePage
moveToNextAttacker bp = selectToCurrentAttacker $ startCountDownIfEnemyAttacking
  bp{currentAttacker = nxtAttacker}
  where nxtAttacker = atkSeq !! nxtIdx
        nxtIdx = (curIdx + 1) `mod` (length atkSeq)
        curIdx = fromJust $ findIndex (\s -> s == currentAttacker bp) atkSeq
        atkSeq = attackSequence bp

selectToCurrentAttacker :: BattlePage -> BattlePage
selectToCurrentAttacker bp = selectFunc (currentAttacker bp) bp{state = newState}
  where (selectFunc, newState) = case isEnemyAttackNext bp of
          True -> (selectEnemy, EnemyAttacking)
          False -> (selectHero, HeroTurn)

startCountDownIfEnemyAttacking :: BattlePage -> BattlePage
startCountDownIfEnemyAttacking bp = bp{countDown = newCd}
  where newCd = case isEnemyAttackNext bp of
                  True -> attackFrame bp
                  _ -> 0

handleSelectUp :: BattlePage -> BattlePage
handleSelectUp bp@BattlePage{
  state = HeroTurn,
  enemies = es
  } = bp {
  enemies = L.listMoveUp es
  }
handleSelectUp bp = bp

handleSelectDown :: BattlePage -> BattlePage
handleSelectDown bp@BattlePage{
  state = HeroTurn,
  enemies = es
  } = bp {
  enemies = L.listMoveDown es
  }
handleSelectDown bp = bp

handleConfirmAction :: BattlePage -> BattlePage
handleConfirmAction bp@BattlePage{
  enemies = es,
  state = HeroTurn
  } = performHeroAttack idx bp
  where idx = fromJust $ L.listSelected es
handleConfirmAction bp = bp

handleGameTick :: BattlePage -> BattlePage
handleGameTick bp@BattlePage{
  state = EnemyAttacking,
  countDown = cd
  } = nextAction bp{
  countDown = newCd
  }
  where newCd = max 0 $ cd - 1
        nextAction = case newCd of
          0 -> enemyAttackRandomHero
          _ -> id
handleGameTick bp = bp

generateBattleResult :: BattlePage -> BattleResultPage
generateBattleResult BattlePage{
  totalReward = totalRwd,
  heros = hs,
  deadHeros = dhs
  } = BattleResultPage{result = res}
  where res = DG.BattleResult{
          DG.money = totalRwd,
          DG.updatedHero = L.list Normal (L.listElements hs Vec.++ L.listElements dhs ) 1
          }
