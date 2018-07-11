module Core.Enemy where

type Name = String
type HP = Int
type MaxHP = Int
type Atk = Int
type ExpReward = Int
type MoneyReward = Int

data Enemy = Enemy {
  name :: Name,
  maxHP :: MaxHP,
  hp :: HP,
  atk :: Atk,
  expReward :: ExpReward,
  moneyReward :: MoneyReward
  } deriving (Show)
