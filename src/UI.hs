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
import Core.DungeonsPage
import Core.DungeonPrepPage
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
              $ L.renderList drawStringList True
              $ fmap show hs

drawDungeonsPage :: World -> [Widget CursorName]
drawDungeonsPage w@World{ dungeonsPage = dp@DungeonsPage{dungeons = ds}} = [vBox [box]]
  where box = B.borderWithLabel (str "Dungeons")
              $ C.hCenter
              $ L.renderList drawStringList True
              $ fmap show ds

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
              $ L.renderList drawBenchList True
              $ fmap show benchHeros
        teamBox = B.borderWithLabel (str "Team")
              $ C.hCenter
              $ L.renderList drawTeamList True
              $ fmap show team
        (drawBenchList, drawTeamList) = case mode of
          Add -> (drawStringList, drawStringListNoSelect)
          Remove -> (drawStringListNoSelect, drawStringList)

drawStringListNoSelect :: Bool -> String -> Widget CursorName
drawStringListNoSelect sel a = C.hCenter $ str a

drawStringList :: Bool -> String -> Widget CursorName
drawStringList sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "* <" <> s <> ">")
                   else str s
    in C.hCenter $ selStr $ a

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"

