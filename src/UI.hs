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

drawMain :: World -> [Widget Name]
drawMain w = [vBox [box]]
  where box = withBorderStyle BS.unicodeBold
              $ B.borderWithLabel (str "Brightest Dungeon")
              $ C.hCenter
              $ padAll 1
              $ L.renderList drawStringList True
              $ options w

drawHeroPage :: World -> [Widget Name]
drawHeroPage w@World{heros = hs} = [vBox [box]]
  where box = B.borderWithLabel (str "Heros")
              $ C.hCenter
              $ L.renderList drawStringList True
              $ L.list () (Vec.fromList $ map show hs) 1

drawDungeonsPage :: World -> [Widget Name]
drawDungeonsPage w@World{ dungeonsPage = dp@DungeonsPage{dungeons = ds}} = [vBox [box]]
  where box = B.borderWithLabel (str "Dungeons")
              $ C.hCenter
              $ L.renderList drawStringList True
              $ fmap show ds

drawStringList :: Bool -> String -> Widget Name
drawStringList sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "* " <+> (selStr $ a)

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"

