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
import Core.Equipment
import Core.InventoryPage(InventoryPage(..))

import UI.Common
import UI.BattlePageUI

drawMain :: World -> [Widget CursorName]
drawMain w = [vBox [box]]
  where box = withBorderStyle BS.unicodeBold
              $ B.borderWithLabel (str "Brightest Dungeon")
              $ C.hCenter
              $ padAll 1
              $ vBox [summary, padTop (Pad 2) $ vBox [renderOptions]]
        renderOptions =
              L.renderList renderStringWrappedInBox True
              $ fmap show $ options w
        summary =
          renderBoxWithName "Summary" False
          $ C.hCenter
          $ hBox $ fmap str [
          "Wealth: " ++ (show $ wealth w)
          ]

drawHeroPage :: World -> [Widget CursorName]
drawHeroPage World{heros = hs} = [vBox [box]]
  where box = B.borderWithLabel (str "Heros")
              $ C.hCenter
              $ L.renderList drawHero True hs

drawDungeonsPage :: World -> [Widget CursorName]
drawDungeonsPage World{ dungeonsPage = DungeonsPage{dungeons = ds}} = [vBox [box]]
  where box = B.borderWithLabel (str "Dungeons")
              $ C.hCenter
              $ L.renderList drawDungeon True
              $ ds

drawDungeonPreparePage :: World -> [Widget CursorName]
drawDungeonPreparePage World{ dungeonPrep = dpp} = drawDungeonPreparePage_ dpp

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
drawFightResultScene World{
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

drawInventoryPage :: World -> [Widget CursorName]
drawInventoryPage World {
  inventoryPage = InventoryPage{allEquipments = invs}
  } = [vBox [box]]
  where box = B.borderWithLabel (str "All Inventories")
              $ C.hCenter
              $ L.renderList drawEquipment True invs

drawEquipment :: Bool -> Equipment -> Widget CursorName
drawEquipment sel Equipment{
  equipName = eName
  } = renderBoxWithName eName sel
      $ C.hCenter $ vBox []

drawDungeon :: Bool -> Dungeon -> Widget CursorName
drawDungeon sel Dungeon{
  dName = nm,
  enemies = es,
  missionLength = missonL,
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
  [swordUnicode] ++ (show $ h^.totalAtk),
  "level: " ++ (show $ h^.level),
  "exp cap" ++ (show $ h^.expCap),
  "exp " ++ (show $ h^.curExp),
  renderProgressBar (h^.curExp ) (h^.expCap )
  ]


drawUI :: World -> [Widget CursorName]
drawUI w@(World{currentScene = scene}) =
  case scene of Main -> drawMain w
                HeroInfoScene -> drawHeroPage w
                DungeonSelectionScene -> drawDungeonsPage w
                DungeonPrepareScene -> drawDungeonPreparePage w
                FightResultScene -> drawFightResultScene w
                FightScene -> drawBattlePage $ fromJust $ battlePage w
                InventoryScene -> drawInventoryPage w
                -- _ -> [vBox [str $ show scene]]

