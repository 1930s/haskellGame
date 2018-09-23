module Core.Hero where

data Hero = Hero {
  name :: String,
  maxHP :: Int,
  hp :: Int,
  atk :: Int,
  level :: Int,
  curExp :: Int,
  expCap :: Int
  } deriving (Ord)

isHeroAlive :: Hero -> Bool
isHeroAlive h = hp h > 0

heroTakeAttack :: Int -> Hero -> Hero
heroTakeAttack dmg h = h{hp = max 0 (hp h - dmg)}

heroRevive :: Hero -> Hero
heroRevive h = h{hp = maxHP h}

-- should property test this
heroReceiveExp :: Int -> Hero -> Hero
heroReceiveExp amt h = h{level = lvl, curExp = cExp, expCap = eCap}
  where (lvl, cExp, eCap) = until noLevelUp oneLevelUp (level h, curExp h + amt, expCap h)
        noLevelUp (_, e, cap) = e < cap
        oneLevelUp (l, e, cap) = (l+1, e-cap, cap*2)

instance Eq Hero where
  a == b = name a == name b
