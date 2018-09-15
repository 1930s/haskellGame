module Input where
import qualified Graphics.Vty as V
import Brick
  ( BrickEvent(..))

type Name = ()
data Tick = Tick

data Input = CharKey Char
           | Enter
           | KeyUP
           | KeyDown
           | NumKey Int
           | KeyEsc
           deriving (Eq, Show, Read)

brickEventToInput :: BrickEvent Name Tick -> Maybe Input
brickEventToInput (VtyEvent (V.EvKey (V.KChar '1') [])) = Just $ NumKey 1
brickEventToInput (VtyEvent (V.EvKey (V.KChar '2') [])) = Just $ NumKey 2
brickEventToInput (VtyEvent (V.EvKey (V.KChar '3') [])) = Just $ NumKey 3
brickEventToInput (VtyEvent (V.EvKey (V.KChar '4') [])) = Just $ NumKey 4
brickEventToInput (VtyEvent (V.EvKey (V.KEnter) [])) = Just $ Enter
brickEventToInput (VtyEvent (V.EvKey (V.KEsc) [])) = Just $ KeyEsc
brickEventToInput (VtyEvent (V.EvKey (V.KUp) [])) = Just $ KeyUP
brickEventToInput (VtyEvent (V.EvKey (V.KDown) [])) = Just $ KeyDown
brickEventToInput (VtyEvent (V.EvKey (V.KChar ch) [])) = Just $ CharKey ch
brickEventToInput _ = Nothing
