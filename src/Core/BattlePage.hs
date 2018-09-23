module Core.BattlePage where

import System.Random
import Input
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

data BattlePage = BattlePage{
  heros :: L.List U.CursorName Hero,
  enemies :: L.List U.CursorName Enemy,
  attackSequence :: [String],
  state :: BattleState,
  totalReward :: Int,
  attackFrame :: Int,
  cd :: Int,
  randomGen :: StdGen
  }

initialiseBattlePage :: L.List U.CursorName Hero
                     -> L.List U.CursorName Enemy
                     -> StdGen
                     -> Int
                     -> BattlePage
initialiseBattlePage hs es gen frame = BattlePage{
  heros = hs,
  enemies = es,
  attackSequence = atkSeq,
  state = HeroTurn,
  totalReward = 0,
  attackFrame = frame,
  cd = 0,
  randomGen = gen}
  where atkSeq = Vec.toList
                 $ (fmap (\h -> Core.Hero.name h) $ L.listElements hs)
                 Vec.++
                 (fmap (\e -> Core.Enemy.name e) $ L.listElements es)

-- enemy in sequence attack a random enemy
enemyAttackRandomHero :: String -> BattlePage -> BattlePage
enemyAttackRandomHero eName bp@BattlePage{
  heros = hs,
  randomGen = generator,
  attackSequence = atkSeq
  } = bp{heros = updatedHeros, attackSequence = updatedSequence}
  where updatedHeros = listReplaceAt heroUnderAttackIdx updatedHero hs
        updatedSequence = case isHeroAlive updatedHero of
          True -> atkSeq
          False -> atkSeq \\ [Core.Hero.name heroUnderAttack]
        enemy = getEnemyFromAttackSequence eName bp
        updatedHero = heroTakeAttack (Core.Enemy.atk enemy) heroUnderAttack
        heroUnderAttack = L.listElements hs Vec.! heroUnderAttackIdx
        (heroUnderAttackIdx, newGen) = randomR (0, length hs) generator

-- enemy take dmg
-- hero get reward if enemyDead
-- change attackSequence
performHeroAttack :: Int -> BattlePage -> BattlePage
performHeroAttack enemyIdx bp@BattlePage{
  heros = hs,
  enemies = es,
  totalReward = mny,
  attackSequence = atkSeq
  } = bp{ enemies = updatedEnemies, heros = updatedHeros}
  where updatedEnemies = listReplaceAt enemyIdx enemyAfterAttack es
        updatedHeros = L.listModify (\_ -> heroAfterAttack) hs
        (_, attackHero) = fromJust $ L.listSelectedElement hs
        enemyUnderAttack = (L.listElements es) Vec.! enemyIdx
        enemyAfterAttack = enemyTakeAttack (Core.Hero.atk attackHero) enemyUnderAttack
        heroAfterAttack = heroReceiveExp expAwd attackHero
        (expAwd, updatedAttackSeq)  = case (isAlive enemyAfterAttack) of
          False -> (expReward enemyAfterAttack, atkSeq \\ [Core.Enemy.name enemyAfterAttack])
          _ -> (0, atkSeq)

selectEnemy :: Input -> BattlePage -> BattlePage
selectEnemy inp bp = case inp of
                       KeyUp -> bp{enemies = L.listMoveUp $ enemies bp}
                       KeyDown -> bp{enemies = L.listMoveDown $ enemies bp}
                       _ -> bp

selectHero :: String -> BattlePage -> BattlePage
selectHero nm bp = bp{heros = L.listMoveTo idx $ heros bp}
  where idx = fromJust $ Vec.findIndex (\h -> Core.Hero.name h == nm)
              $ L.listElements $ heros bp

getEnemyFromAttackSequence :: String -> BattlePage -> Enemy
getEnemyFromAttackSequence s bp = fromJust
                                  $ Vec.find nameMatch
                                  $ L.listElements $ enemies bp
  where nameMatch e = Core.Enemy.name e == s

