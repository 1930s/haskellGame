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
  } deriving (Show, Eq, Ord)

defaultEnemy :: String -> Enemy
defaultEnemy n = Enemy {
            name = n,
            maxHP = 5,
            hp = 5,
            atk = 1,
            expReward = 5,
            moneyReward = 10
            }

