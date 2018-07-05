module Input where

data Input = North
           | South
           | West
           | East
           | Enter
           | Exit
           deriving (Eq, Show)

getInput :: IO (Input)
getInput = do
  key <- getChar
  case key of
    'w' -> return North
    's' -> return South
    'a' -> return West
    'd' -> return East
    'q' -> return Exit
    _ -> getInput

