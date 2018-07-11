module Input where

import Data.Map as Map
import Text.Read
import Data.Char

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
           | Input Int
           deriving (Eq, Show, Read)

getInput :: IO (Input)
getInput = do
  key <- getChar
  if isNumber key then return (Input $ digitToInt key) else
    case (readMaybe [toUpper key] :: Maybe Input) of
      Just i -> return i
      Nothing -> getInput

