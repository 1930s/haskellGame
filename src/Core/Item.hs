module Core.Item where

data ItemType = Gold
              | Sword
              deriving(Eq, Show)

data Item = Item {
  name :: String,
  price :: Int
                 }
