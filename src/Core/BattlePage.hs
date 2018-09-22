module BattlePage where

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Core.Utils as U
import Core.Hero
import Core.Enemy

data BattleState = HeroTurn
                 | EnemyAttacking

data BattlePage = BattlePage{
  heros :: L.List U.CursorName Hero,
  enemies :: L.List U.CursorName Enemy,
  attackSequence :: [(Int, Int)],
  state :: BattleState,
  attackFrame :: Int,
  cd :: Int
  }

performHeroAttack :: Int -> BattlePage -> BattlePage
performHeroAttack enemyIdx bp = bp
