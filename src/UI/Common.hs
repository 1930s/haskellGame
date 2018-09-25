{-# LANGUAGE OverloadedStrings #-}
module UI.Common where

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

drawStringListNoSelect :: Bool -> String -> Widget CursorName
drawStringListNoSelect _ a = drawStringList False a

drawStringList :: Bool -> String -> Widget CursorName
drawStringList sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ [selectUnicode] <> " <" <> s <> ">")
                   else str s
    in C.hCenter $ selStr $ a

selectUnicode:: Char
selectUnicode = '\10144'

heartUnicode :: Char
heartUnicode = '\9829'

swordUnicode :: Char
swordUnicode = '\9876'

healthBarUnicode :: Char
healthBarUnicode = '\10074'

healthEmptyBarUnicode :: Char
healthEmptyBarUnicode = '\10072'

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

renderHealthBar :: Int -> Int -> String
renderHealthBar p m = (take p $ repeat healthBarUnicode) ++ (take (m - p) $ repeat healthEmptyBarUnicode)
