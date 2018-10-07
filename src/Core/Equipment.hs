module Core.Equipment where

data EquipType = Offence
               | Defence
               deriving(Show, Eq)

data Equipment = Equipment {
  equipName :: String,
  equipType :: EquipType,
  equipPrice :: Int,
  equipValue :: Int
  }
