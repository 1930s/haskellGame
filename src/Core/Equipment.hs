module Core.Equipment where

data EquipType = Offence
               | Defence
               deriving(Show, Eq, Ord)

data Equipment = Equipment {
  equipName :: String,
  equipType :: EquipType,
  equipPrice :: Int,
  equipValue :: Int
  } deriving (Eq, Ord)
