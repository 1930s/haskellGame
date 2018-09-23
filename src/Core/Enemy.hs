module Core.Enemy where

data Enemy = Enemy {
  name :: String,
  maxHP :: Int,
  hp :: Int,
  atk :: Int,
  expReward :: Int,
  moneyReward :: Int
  } deriving (Show, Eq, Ord)

isAlive :: Enemy -> Bool
isAlive e = hp e > 0

enemyTakeAttack :: Int -> Enemy -> Enemy
enemyTakeAttack dmg e = e{hp = max 0 (hp e - dmg)}

defaultEnemy :: String -> Enemy
defaultEnemy n = Enemy {
            name = n,
            maxHP = 5,
            hp = 5,
            atk = 1,
            expReward = 5,
            moneyReward = 10
            }

