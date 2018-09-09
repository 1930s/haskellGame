{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( game
    ) where

import System.Random
import Core.World
import HandleInput
import Input

import Data.Monoid

import qualified Data.Vector as Vec
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import Brick.BChan(newBChan, writeBChan)
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )

oneSecond :: Int
oneSecond = (10::Int) ^ (6::Int)


app :: App World Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

game :: IO ()
game = do
  rGen <- getStdGen
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay $ oneSecond `div` 10
  let g = defaultWorld rGen
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

handleEvent :: World -> BrickEvent Name Tick -> EventM Name (Next World)
handleEvent w (AppEvent Tick) = continue $ gameTick w
handleEvent w (VtyEvent (V.EvKey V.KEsc [])) = halt w
handleEvent w e =
  case brickEventToInput e of
    Nothing -> continue w
    Just i -> continue $ handleGameInput w i

drawUI :: World -> [Widget Name]
drawUI w = [vBox [box]]
  where box = withBorderStyle BS.unicodeBold
          $ B.borderWithLabel (str "Brightest Dungeon")
          $ C.hCenter
          $ padAll 1
          $ L.renderList drawStringList True
          $ L.list () (Vec.fromList ["option1 ", "option2", "option3"]) 1

drawStringList :: (Show a) => Bool -> a -> Widget Name
drawStringList sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "* " <+> (selStr $ show a)

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: AttrMap
theMap = attrMap V.defAttr []


