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
  } deriving (Ord)

instance Eq Hero where
  a == b = name a == name b

instance Show Hero where
  show (Hero {name = n, hp = h, atk = a, curExp = cur_exp, level = lvl}) = intercalate "\n" toDisplay
    where show_name = "Name: " ++ show n
          show_hp = "HP: " ++ show h
          show_atk = "ATK: " ++ show a
          show_exp = "Exp: " ++ show cur_exp
          show_level = "Level: " ++ show lvl
          toDisplay = ["Hero:", show_name, show_hp, show_atk, show_exp, show_level, "--"]
