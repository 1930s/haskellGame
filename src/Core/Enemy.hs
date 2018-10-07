{-# LANGUAGE TemplateHaskell #-}

module Core.Enemy where

import Control.Lens

data Enemy = Enemy {
  _eName :: String,
  _eMaxHP :: Int,
  _eHp :: Int,
  _eAtk :: Int,
  _eExpReward :: Int,
  _eMoneyReward :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''Enemy

isAlive :: Enemy -> Bool
isAlive e = e^.eHp > 0

enemyTakeAttack :: Int -> Enemy -> Enemy
enemyTakeAttack dmg e = set eHp (max 0 (e^.eHp - dmg)) e

defaultEnemy :: String -> Enemy
defaultEnemy n = Enemy {
            _eName = n,
            _eMaxHP = 5,
            _eHp = 5,
            _eAtk = 2,
            _eExpReward = 10,
            _eMoneyReward = 10
            }

