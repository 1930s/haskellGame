{-# LANGUAGE DuplicateRecordFields #-}

module UI.Main where

import Control.Lens
import Data.Maybe
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Brick
  ( Widget
  , vBox, hBox
  , padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  )
import Core.World
import Core.Hero
import Core.Dungeon
import Core.DungeonsPage
import Core.DungeonPrepPage
import Core.BattleResultPage
import Core.Utils

import UI.Common
import UI.BattlePageUI

drawMain :: World -> [Widget CursorName]
drawMain w = [vBox [box]]
  where box = withBorderStyle BS.unicodeBold
              $ B.borderWithLabel (str "Brightest Dungeon")
              $ C.hCenter
              $ padAll 1
              $ vBox [ summary, renderOptions ]
        renderOptions =
              L.renderList drawStringList True
              $ options w
        summary =
          renderBoxWithName "Summary" False
          $ C.hCenter
          $ hBox $ fmap str [
          "Wealth: " ++ (show $ wealth w)
          ]

drawHeroPage :: World -> [Widget CursorName]
drawHeroPage w@World{heros = hs} = [vBox [box]]
  where box = B.borderWithLabel (str "Heros")
              $ C.hCenter
              $ L.renderList drawHero True hs

drawDungeonsPage :: World -> [Widget CursorName]
drawDungeonsPage w@World{ dungeonsPage = dp@DungeonsPage{dungeons = ds}} = [vBox [box]]
  where box = B.borderWithLabel (str "Dungeons")
              $ C.hCenter
              $ L.renderList drawDungeon True
              $ ds

drawDungeonPreparePage :: World -> [Widget CursorName]
drawDungeonPreparePage w@World{ dungeonPrep = dpp} = drawDungeonPreparePage_ dpp

drawDungeonPreparePage_ :: DungeonPrepPage -> [Widget CursorName]
drawDungeonPreparePage_ DungeonPrepPage{
  benchHeros = benchHs,
  team = tm,
  mode = md
  } = [vBox [hBox [teamBox, benchBox], str "Press s to start mission"]]
  where benchBox = B.borderWithLabel (str "Bench heros")
              $ C.hCenter
              $ L.renderList drawBenchList True benchHs
        teamBox = B.borderWithLabel (str "Team")
              $ C.hCenter
              $ L.renderList drawTeamList True tm
        (drawBenchList, drawTeamList) = case md of
          Add -> (drawHero, drawHeroNoSelect)
          Remove -> (drawHeroNoSelect, drawHero)

drawFightResultScene :: World -> [Widget CursorName]
drawFightResultScene w@World{
  battleResultPage = bRP } = [vBox [summaryBox, heroBox]]
  where res = result bRP
        hs = updatedHero res
        heroBox =
          padTop (Pad 3)
          $ B.borderWithLabel (str "Updated heros")
          $ C.hCenter
          $ L.renderList drawHeroNoSelect True hs
        summaryBox =
          B.borderWithLabel (str "Summary")
          $ C.hCenter
          $ str $ "Money" ++ (show $ money res)

drawDungeon :: Bool -> Dungeon -> Widget CursorName
drawDungeon sel h@Dungeon{
  dName = nm,
  enemies = es,
  missionLength = missonL,
  herosInDungeon = hs,
  state = stt,
  countDown = cd
  } =
  renderBoxWithName nm sel
  $ C.hCenter
  $ vBox [renderEnemyNum, renderState, str $ renderProgress ]
  where
    renderEnemyNum = str $ "Number of enemies: " ++ (show $ length es)
    renderState = str $ show stt
    renderProgress = case stt of
      InProgress -> renderProgressBar cd missonL
      MissionComplete -> "Completed!"
      _ -> ""

drawHeroNoSelect :: Bool -> Hero -> Widget CursorName
drawHeroNoSelect _ h = drawHero False h

drawHero :: Bool -> Hero -> Widget CursorName
drawHero sel h =
  renderBoxWithName (h^.name) sel
  $ C.hCenter
  $ vBox
  $ fmap str [
  [heartUnicode] ++ (show $ h^.hp),
  [swordUnicode] ++ (show $ h^.atk),
  "level: " ++ (show $ h^.level),
  "exp cap" ++ (show $ h^.expCap),
  "exp " ++ (show $ h^.curExp),
  renderProgressBar (h^.curExp ) (h^.expCap )
  ]

drawUI :: World -> [Widget CursorName]
drawUI w@(World{currentScene = scene}) =
  case scene of Main -> drawMain w
                HeroInfo -> drawHeroPage w
                Dungeons -> drawDungeonsPage w
                DungeonPrepare -> drawDungeonPreparePage w
                FightResultScene -> drawFightResultScene w
                FightScene -> drawBattlePage $ fromJust $battlePage w
                -- _ -> [vBox [str $ show scene]]

