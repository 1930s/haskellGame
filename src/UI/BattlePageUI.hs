module UI.BattlePageUI where

import Core.Hero
import Core.Enemy
import Core.BattlePage
import Core.Utils (CursorName)
import qualified Brick.Widgets.Center as C
import Brick
  ( Widget
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import UI.Common
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec


drawBattlePage :: BattlePage -> [Widget CursorName]
drawBattlePage bp@BattlePage{
  heros = hs,
  enemies = es,
  attackSequence = atkSeq,
  state = stt,
  currentAttacker = curAtk,
  countDown = cd
  } = [vBox [stateBox, battleBoxes]]
  where stateBox = vBox $ fmap str [show stt, show atkSeq, show curAtk, "frame: " ++ show cd]
        battleBoxes = hBox [teamBox, enemiesBox]
        teamBox = renderBoxWithName "Team" False
                  $ C.hCenter
                  $ L.renderList drawBattleHero True hs
        enemiesBox = renderBoxWithName "Enemies" False
                  $ C.hCenter
                  $ L.renderList drawBattleEnemy True es


drawBattleEnemy :: Bool -> Enemy -> Widget CursorName
drawBattleEnemy sel e@Enemy{
  Core.Enemy.name = nm,
  Core.Enemy.atk = attack,
  Core.Enemy.maxHP = maxHealth,
  Core.Enemy.hp = health
  } = renderBoxWithName nm sel
      $ C.hCenter
      $ vBox
      $ fmap str
      [
        [swordUnicode] ++ (show $ attack),
        renderHealthBar health maxHealth
      ]

drawBattleHero :: Bool -> Hero -> Widget CursorName
drawBattleHero sel h@Hero{
  Core.Hero.name = nm,
  Core.Hero.atk = attack,
  Core.Hero.maxHP = maxHealth,
  Core.Hero.hp = health
  } = renderBoxWithName nm sel
      $ C.hCenter
      $ vBox
      $ fmap str
      [
        [swordUnicode] ++ (show $ attack),
        renderHealthBar health maxHealth
      ]
