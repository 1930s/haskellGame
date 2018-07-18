module Input where

import Text.Read
import Data.Char
import qualified Text.Read.Lex as L

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
           | Enter
           | Input Int
           deriving (Eq, Show)

instance Read Input where
  readsPrec _ val = readF [("A",A)
                          ,("B",B)
                          ,("C",C)
                          ,("D",D)
                          ,("E",E)
                          ,("F",F)
                          ,("G",G)
                          ,("H",H)
                          ,("W",W)
                          ,("S",S)
                          ,("Q",Q)
                          ,("M",M)
                          ,("J",J)
                          ,("K",K)
                          ,("\n",Enter)
                          ,(" ",Enter)
                          ,("1",Input 1)
                          ,("2",Input 2)
                          ,("3",Input 3)
                          ,("4",Input 4)
                          ,("5",Input 5)
                          ,("6",Input 6)
                          ,("7",Input 7)
                          ,("8",Input 8)
                          ,("9",Input 9)
                          ]
    where readF [] = []
          readF ((attempt, result):xs) =
            if (take (length attempt) val) == attempt
                         then [(result, drop (length attempt) val)]
                         else readF xs

getInput :: IO (Input)
getInput = do
  key <- getChar
  if isNumber key then return (Input $ digitToInt key) else
    case (readMaybe [toUpper key] :: Maybe Input) of
      Just i -> return i
      Nothing -> getInput

