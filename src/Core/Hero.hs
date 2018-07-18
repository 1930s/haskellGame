module Core.Hero where

import Data.List

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
  } deriving (Eq, Ord)

instance Show Hero where
  show (Hero {name = n, hp = h, atk = a}) = intercalate "\n" toDisplay
    where show_name = "Name: " ++ show n
          show_hp = "HP: " ++ show h
          show_atk = "ATK: " ++ show a
          toDisplay = ["Hero:", show_name, show_hp, show_atk, "--"]
