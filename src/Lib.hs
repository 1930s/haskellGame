{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( game
    ) where

import System.Random
import Core.Utils(CursorName(..))
import Core.World
import HandleInput
import Input


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
import UI

oneSecond :: Int
oneSecond = (10::Int) ^ (6::Int)


app :: App World Tick CursorName
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

handleEvent :: World -> BrickEvent CursorName Tick -> EventM CursorName (Next World)
handleEvent w (AppEvent Tick) = continue $ gameTick w
-- handleEvent w@World{currentScene = Main} (VtyEvent (V.EvKey V.KEsc [])) = halt w
handleEvent w (VtyEvent (V.EvKey V.KEsc [])) = halt w
handleEvent w e =
  case brickEventToInput e of
    Just i -> continue $ handleGameInput w i
    _ -> continue w

drawUI :: World -> [Widget CursorName]
drawUI w@(World{currentScene = scene}) =
  case scene of Main -> drawMain w
                HeroInfo -> drawHeroPage w
                Dungeons -> drawDungeonsPage w
                DungeonPrepare -> drawDungeonPreparePage w
                _ -> [vBox [str $ show scene]]

theMap :: AttrMap
theMap = attrMap V.defAttr []


