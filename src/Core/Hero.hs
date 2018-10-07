{-# LANGUAGE TemplateHaskell#-}

module Core.Hero(Hero,
                 isHeroAlive,
                 heroTakeAttack,
                 heroRevive,
                 heroReceiveExp,
                 defaultHero,
                 name,
                 maxHP,
                 hp,
                 totalAtk,
                 totalDef,
                 level,
                 curExp,
                 expCap,
                 equipments
                ) where

import Control.Lens

import Core.Equipment

data Hero = Hero {
  _name :: String,
  _maxHP :: Int,
  _hp :: Int,
  _atk :: Int,
  _def :: Int,
  _level :: Int,
  _curExp :: Int,
  _expCap :: Int,
  _equipments :: [Equipment]
  }

makeLenses ''Hero

totalAtk :: Lens' Hero Int
totalAtk = lens (\h -> _calculateTotalAtk h) (\h _ -> h)

totalDef :: Lens' Hero Int
totalDef = lens (\h -> _calculateTotalDef h) (\h _ -> h)

_calculateTotalAtk :: Hero -> Int
_calculateTotalAtk Hero{
  _atk = baseAtk,
  _equipments = equips
  } = baseAtk + (sum $ fmap equipValue equips)
  where offenceEquips = filter (\e -> equipType e == Offence) equips

_calculateTotalDef :: Hero -> Int
_calculateTotalDef Hero{
  _def = baseDef,
  _equipments = equips
  } = baseDef + (sum $ fmap equipValue equips)
  where offenceEquips = filter (\e -> equipType e == Defence) equips

isHeroAlive :: Hero -> Bool
isHeroAlive h = h^.hp > 0

heroTakeAttack :: Int -> Hero -> Hero
heroTakeAttack dmg h = set hp (max 0 (h^.hp - realDmg)) h
  where realDmg = max 0 (dmg - h^.totalDef)

heroRevive :: Hero -> Hero
heroRevive h = set hp (h^.maxHP) h

-- should property test this
heroReceiveExp :: Int -> Hero -> Hero
heroReceiveExp amt h = set level lvl $ set curExp cExp $ set expCap eCap h
  where (lvl, cExp, eCap) = until noLevelUp oneLevelUp (h^.level, h^.curExp  + amt, h^.expCap )
        noLevelUp (_, e, cap) = e < cap
        oneLevelUp (l, e, cap) = (l+1, e-cap, cap*2)

instance Eq Hero where
  a == b = a^.name == b^.name

defaultHero :: String -> Hero
defaultHero nm = Hero{
  _name = nm,
  _maxHP = 10,
  _hp = 10,
  _atk = 2,
  _def = 1,
  _level = 1,
  _curExp = 0,
  _expCap = 10,
  _equipments = []
  }
