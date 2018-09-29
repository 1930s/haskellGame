module Core.BattlePage(
  BattlePage(..),
  initialiseBattlePage,
  handleSelectDown,
  handleSelectUp,
  handleConfirmAction,
  handleGameTick
  ) where

import System.Random
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Core.Utils as U
import Core.Hero
import Core.Enemy
import Core.Utils

import Data.List
import Data.Maybe

data BattleState = HeroTurn
                 | EnemyAttacking
                 deriving(Show)

data BattlePage = BattlePage{
  heros :: L.List U.CursorName Hero,
  enemies :: L.List U.CursorName Enemy,
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
  enemies = es,
  attackSequence = atkSeq,
  currentAttacker = curAttacker,
  state = HeroTurn,
  totalReward = 0,
  attackFrame = frame,
  countDown = 0,
  randomGen = gen}
  where curAttacker = head atkSeq
        atkSeq = Vec.toList
                 $ (fmap (\h -> Core.Hero.name h) $ L.listElements hs)
                 Vec.++
                 (fmap (\e -> Core.Enemy.name e) $ L.listElements es)

-- enemy in sequence attack a random enemy
enemyAttackRandomHero :: BattlePage -> BattlePage
enemyAttackRandomHero bp@BattlePage{state = HeroTurn} = bp
enemyAttackRandomHero bp@BattlePage{
  state = EnemyAttacking,
  heros = hs,
  randomGen = generator,
  attackSequence = atkSeq,
  currentAttacker = curAttacker
  } = moveToNextAttacker bp{heros = updatedHeros
        , attackSequence = updatedSequence
        , randomGen = newGen}
  where updatedHeros = listReplaceAt heroUnderAttackIdx updatedHero hs
        updatedSequence = case isHeroAlive updatedHero of
          True -> atkSeq
          False -> atkSeq \\ [Core.Hero.name heroUnderAttack]
        enemy = getEnemyFromAttackSequence curAttacker bp
        updatedHero = heroTakeAttack (Core.Enemy.atk enemy) heroUnderAttack
        heroUnderAttack = L.listElements hs Vec.! heroUnderAttackIdx
        (heroUnderAttackIdx, newGen) = randomR (0, length hs -1) generator

-- enemy take dmg
-- hero get reward if enemyDead
-- change attackSequence
performHeroAttack :: Int -> BattlePage -> BattlePage
performHeroAttack enemyIdx bp@BattlePage{
  heros = hs,
  enemies = es,
  totalReward = mny,
  attackSequence = atkSeq
  } = bp{ enemies = updatedEnemies
        , heros = updatedHeros
        , attackSequence = updatedAttackSeq
        , totalReward = mny+mnyAwd}
  where updatedEnemies = listReplaceAt enemyIdx enemyAfterAttack es
        updatedHeros = L.listModify (\_ -> heroAfterAttack) hs
        (_, attackHero) = fromJust $ L.listSelectedElement hs
        enemyUnderAttack = (L.listElements es) Vec.! enemyIdx
        enemyAfterAttack = enemyTakeAttack (Core.Hero.atk attackHero) enemyUnderAttack
        heroAfterAttack = heroReceiveExp expAwd attackHero
        (mnyAwd, expAwd, updatedAttackSeq)  = case (isAlive enemyAfterAttack) of
          False -> (moneyReward enemyAfterAttack
                   , expReward enemyAfterAttack
                   , atkSeq \\ [Core.Enemy.name enemyAfterAttack])
          _ -> (0, 0, atkSeq)

selectHero :: String -> BattlePage -> BattlePage
selectHero nm bp = bp{heros = L.listMoveTo idx $ heros bp}
  where idx = fromJust $ Vec.findIndex (\h -> Core.Hero.name h == nm)
              $ L.listElements $ heros bp

selectEnemy :: String -> BattlePage -> BattlePage
selectEnemy nm bp = bp{enemies = L.listMoveTo idx $ enemies bp}
  where idx = fromJust $ Vec.findIndex (\h -> Core.Enemy.name h == nm)
              $ L.listElements $ enemies bp

getEnemyFromAttackSequence :: String -> BattlePage -> Enemy
getEnemyFromAttackSequence s bp = fromJust
                                  $ Vec.find nameMatch
                                  $ L.listElements $ enemies bp
  where nameMatch e = Core.Enemy.name e == s

isEnemyAttackNext :: BattlePage -> Bool
isEnemyAttackNext bp =
  case res of
    Nothing -> False
    Just _ -> True
  where res = Vec.findIndex matchFunc $ L.listElements $ enemies bp
        matchFunc e = Core.Enemy.name e == currentAttacker bp

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
  } = moveToNextAttacker $ performHeroAttack idx bp
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
