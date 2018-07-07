module Input where

type Input = Char

getInput :: IO (Input)
getInput = do
  key <- getChar
  if (key >= 'a' && key <= 'z') || (key >= '0' && key <= '9')
    then return key
    else getInput

