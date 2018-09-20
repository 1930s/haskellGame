module Core.Hero where

type Name = String
type HP = Int
type MaxHP = Int
type Atk = Int
type Level = Int
type CurExp = Int
type ExpCup = Int

data Hero = Hero {
  name :: Name,
  maxHP :: MaxHP,
  hp :: HP,
  atk :: Atk,
  level :: Level,
  curExp :: CurExp,
  expCap :: ExpCup
  } deriving (Ord)

instance Eq Hero where
  a == b = name a == name b
