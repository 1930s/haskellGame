module Enemy where

type Name = String
type HP = Int
type Atk = Int
type ExpReward = Int
type MoneyReward = Int

data Enemy = Enemy {
  name :: Name,
  hp :: HP,
  atk :: Atk,
  expReward :: ExpReward,
  moneyReward :: MoneyReward
  } deriving (Show)
