module Input where
import qualified Graphics.Vty as V
import Brick
  ( BrickEvent(..))

type Name = ()
data Tick = Tick
data Input = A
           | B
           | C
           | D
           | E
           | F
           | G
           | H
           | W
           | S
           | Q
           | M
           | J
           | K
           | R
           | Enter
           | Input Int
           deriving (Eq, Show, Read)

brickEventToInput :: BrickEvent Name Tick -> Maybe Input
brickEventToInput (VtyEvent (V.EvKey (V.KChar '1') [])) = Just $ Input 1
brickEventToInput (VtyEvent (V.EvKey (V.KChar '2') [])) = Just $ Input 2
brickEventToInput (VtyEvent (V.EvKey (V.KChar '3') [])) = Just $ Input 3
brickEventToInput (VtyEvent (V.EvKey (V.KChar '4') [])) = Just $ Input 4
brickEventToInput (VtyEvent (V.EvKey (V.KEnter) [])) = Just $ Enter
brickEventToInput (VtyEvent (V.EvKey (V.KChar ch) [])) = Just (read [ch] :: Input)
brickEventToInput _ = Nothing
