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
  state = stt
  } = [hBox [teamBox, enemiesBox]]
  where teamBox = renderBoxWithName "Team" False
                  $ C.hCenter
                  $ vBox
                  $ fmap drawBattleHero
                  $ Vec.toList
                  $ L.listElements hs
        enemiesBox = vBox []


drawBattleHero :: Hero -> Widget CursorName
drawBattleHero h@Hero{
  Core.Hero.name = nm,
  Core.Hero.atk = attack,
  Core.Hero.maxHP = maxHealth,
  Core.Hero.hp = health
  } = renderBoxWithName nm False
      $ C.hCenter
      $ vBox
      $ fmap str
      [
        [swordUnicode] ++ (show $ attack),
        renderHealthBar health maxHealth
      ]
