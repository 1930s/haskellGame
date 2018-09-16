module Input where

import qualified Graphics.Vty as V
import Brick
  ( BrickEvent(..))
import Core.Utils

data Tick = Tick

data Input = CharKey Char
           | Enter
           | KeyUP
           | KeyDown
           | KeyLeft
           | KeyRight
           | NumKey Int
           | KeyEsc
           deriving (Eq, Show, Read)

brickEventToInput :: BrickEvent CursorName Tick -> Maybe Input
brickEventToInput (VtyEvent (V.EvKey (V.KChar '1') [])) = Just $ NumKey 1
brickEventToInput (VtyEvent (V.EvKey (V.KChar '2') [])) = Just $ NumKey 2
brickEventToInput (VtyEvent (V.EvKey (V.KChar '3') [])) = Just $ NumKey 3
brickEventToInput (VtyEvent (V.EvKey (V.KChar '4') [])) = Just $ NumKey 4
brickEventToInput (VtyEvent (V.EvKey (V.KEnter) [])) = Just $ Enter
brickEventToInput (VtyEvent (V.EvKey (V.KEsc) [])) = Just $ KeyEsc
brickEventToInput (VtyEvent (V.EvKey (V.KUp) [])) = Just $ KeyUP
brickEventToInput (VtyEvent (V.EvKey (V.KDown) [])) = Just $ KeyDown
brickEventToInput (VtyEvent (V.EvKey (V.KLeft) [])) = Just $ KeyLeft
brickEventToInput (VtyEvent (V.EvKey (V.KRight) [])) = Just $ KeyRight
brickEventToInput (VtyEvent (V.EvKey (V.KChar ch) [])) = Just $ CharKey ch
brickEventToInput _ = Nothing
