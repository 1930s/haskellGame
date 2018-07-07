module Coord where

import Input

type Coord = (Int, Int)

(|+|) :: Coord -> Coord -> Coord
(|+|) (a,b) (x,y) = (a+x, b+y)

minCoord :: Coord
minCoord = (0,0)

maxCoord :: Coord
maxCoord = (80,80)

restrictCoord :: Coord -> Coord -> Coord -> Coord
restrictCoord (x,y) (xMin, yMin) (xMax, yMax) = (res x xMin xMax, res y yMin yMax)
  where res v vMin vMax = max vMin $ min v vMax

inputToDir :: Input -> Maybe Coord
inputToDir i =
  case i of
    'w'-> Just (0,-1)
    's'-> Just (0,1)
    'a'-> Just (-1,0)
    'd'-> Just (1,0)
    _ -> Nothing

