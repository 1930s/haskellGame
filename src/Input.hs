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
           | SPC
           | Input Int
           deriving (Eq, Show)

-- simplyfy this?
instance Read Input where
  readPrec = parens (
    do L.Ident s <- lexP
       case s of
         "A" -> return A
         "B" -> return B
         "C" -> return C
         "D" -> return D
         "E" -> return E
         "F" -> return F
         "G" -> return G
         "H" -> return H
         "W" -> return W
         "S" -> return S
         "Q" -> return Q
         "M" -> return M
         "J" -> return J
         "K" -> return K
         " " -> return SPC
         "1" -> return $ Input 1
         "2" -> return $ Input 2
         "3" -> return $ Input 3
         "4" -> return $ Input 4
         "5" -> return $ Input 5
         "6" -> return $ Input 6
         "7" -> return $ Input 7
         "8" -> return $ Input 8
         "9" -> return $ Input 9
         _ -> pfail
    )

getInput :: IO (Input)
getInput = do
  key <- getChar
  if isNumber key then return (Input $ digitToInt key) else
    case (readMaybe [toUpper key] :: Maybe Input) of
      Just i -> return i
      Nothing -> getInput

