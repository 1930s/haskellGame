module Core.Equipment where

data EquipType = Offence
               | Defence
               deriving(Show, Eq, Ord)

data Equipment = Equipment {
  equipName :: String,
  equipType :: EquipType,
  equipPrice :: Int,
  equipValue :: Int
  } deriving (Eq, Ord, Show)

defaultEquipment :: String ->  Equipment
defaultEquipment name = Equipment{
  equipName = name,
  equipType = Offence,
  equipPrice = 10,
  equipValue = 1
  }
