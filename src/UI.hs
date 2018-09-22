{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module UI where

import Data.Monoid
import Input
import qualified Data.Vector as Vec
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
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
import Core.World
import Core.Hero
import Core.Dungeon
import Core.DungeonsPage
import Core.DungeonPrepPage
import Core.BattleResultPage
import Core.Utils

drawMain :: World -> [Widget CursorName]
drawMain w = [vBox [box]]
  where box = withBorderStyle BS.unicodeBold
              $ B.borderWithLabel (str "Brightest Dungeon")
              $ C.hCenter
              $ padAll 1
              $ L.renderList drawStringList True
              $ options w

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
  benchHeros = benchHeros,
  team = team,
  mode = mode,
  dungeon = dungeon
  } = [vBox [hBox [teamBox, benchBox], str "Press s to start mission"]]
  where benchBox = B.borderWithLabel (str "Bench heros")
              $ C.hCenter
              $ L.renderList drawBenchList True benchHeros
        teamBox = B.borderWithLabel (str "Team")
              $ C.hCenter
              $ L.renderList drawTeamList True team
        (drawBenchList, drawTeamList) = case mode of
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
  name = nm,
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
drawHero sel h@Hero{name = nm} =
  renderBoxWithName nm sel
  $ C.hCenter
  $ vBox
  $ fmap str [
  heartUnicode ++ (show $ hp h),
  swordUnicode ++ (show $ atk h),
  "level: " ++ (show $ level h),
  renderProgressBar expPer expBarMax
  ]
  where
    expBarMax = 10
    expPer = case (curExp h) of
      0 -> 0
      n -> (expCap h) `div` n


drawStringListNoSelect :: Bool -> String -> Widget CursorName
drawStringListNoSelect _ a = drawStringList False a

drawStringList :: Bool -> String -> Widget CursorName
drawStringList sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ selectUnicode <> " <" <> s <> ">")
                   else str s
    in C.hCenter $ selStr $ a

selectUnicode:: String
selectUnicode = ['\10144']

heartUnicode :: String
heartUnicode = ['\9829']

swordUnicode :: String
swordUnicode = ['\9876']

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"

renderBoxWithName :: String -> Bool -> Widget CursorName -> Widget CursorName
renderBoxWithName nm True w = withBorderStyle
  BS.unicodeBold
  $ B.borderWithLabel (str $ " * " ++ nm )
  $ w
renderBoxWithName nm False w = withBorderStyle
  BS.unicode
  $ B.borderWithLabel (str $ nm )
  $ w

renderProgressBar :: Int -> Int -> String
renderProgressBar p m = (take p $ repeat '#') ++ (take (m - p) $ repeat '-')
