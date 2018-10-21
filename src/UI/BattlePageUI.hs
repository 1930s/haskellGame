module UI.BattlePageUI where

import Control.Lens
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
drawBattlePage BattlePage{
  heros = hs,
  enemies = es,
  attackSequence = atkSeq,
  state = stt,
  currentAttacker = curAtk,
  countDown = cd,
  randomGen = rGen
  } = [vBox [stateBox, battleBoxes]]
  where stateBox = vBox $ fmap str [show stt, show atkSeq, show curAtk, "frame: " ++ show cd,
                                   show rGen]
        battleBoxes = hBox [teamBox, enemiesBox]
        teamBox = renderBoxWithName "Team" False
                  $ C.hCenter
                  $ L.renderList drawBattleHero True hs
        enemiesBox = renderBoxWithName "Enemies" False
                  $ C.hCenter
                  $ L.renderList drawBattleEnemy True es


drawBattleEnemy :: Bool -> Enemy -> Widget CursorName
drawBattleEnemy sel e = renderBoxWithName (e^.eName) sel
      $ C.hCenter
      $ vBox
      $ fmap str
      [
        [swordUnicode] ++ (show $ e^.eAtk),
        renderHealthBar (e^.eHp) (e^.eMaxHP)
      ]

drawBattleHero :: Bool -> Hero -> Widget CursorName
drawBattleHero sel hero =
  renderBoxWithName (hero^.name) sel
  $ C.hCenter
  $ vBox
  $ fmap str
  [
    [swordUnicode] ++ (show $ (hero^.totalAtk)),
    renderHealthBar (hero^.hp) (hero^.maxHP)
  ]
