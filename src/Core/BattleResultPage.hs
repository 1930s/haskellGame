module Core.BattleResultPage where

import Core.Dungeon(BattleResult(..))

data BattleResultPage = BattleResultPage {
  result :: BattleResult
  }

instance Show BattleResultPage where
  show BattleResultPage{result = BattleResult{money = m, updatedHero = hs}}
     = "Money gained: " ++ show m ++ "\n"
       ++ "Heros: " ++ show hs
