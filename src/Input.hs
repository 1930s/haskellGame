module Input where

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
           | Int
           deriving (Eq, Show, Read)

getInput :: IO (Input)
getInput = do
  key <- getChar
  case (readMaybe [toUpper key] :: Maybe Input) of
    Just i -> return i
    Nothing -> getInput
