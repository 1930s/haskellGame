module HandleInput where

import qualified Data.Vector as Vec
import Data.Maybe
import Core.Utils(CursorName(..))
import Core.World
import Core.Hero(Hero(..))
import Core.DungeonPrepPage(addOrRemoveHero,
                            defaultPrepPage,
                            addMode,
                            removeMode,
                            DungeonPrepPage(..))
import Core.DungeonsPage
import Core.Dungeon(DungeonState(..),
                    Dungeon(..),
                    BattleResult(..),
                    -- calcBattle
                   )
import Core.BattleResultPage
import Core.Utils(removeAllEqualElms)
import qualified Brick.Widgets.List as L
import Input

handleGameInput :: World -> Input -> World
handleGameInput w@(World {currentScene=scene}) i =
  case scene of
    Main -> handleMainScene w i
    Dungeons -> handleDungeonScene w i
    DungeonPrepare -> handleDungeonPrepareScene w i
    FightResultScene -> handleFightResultScene w i
    HeroInfo -> handleHeroInfoScene w i
    Fight -> w

handleMainScene :: World -> Input -> World
handleMainScene world inp = nw
  where nw = case inp of
          KeyUP -> world{options = L.listMoveUp $ options world}
          KeyDown -> world{options = L.listMoveDown $ options world}
          Enter -> world{currentScene = nxtScene}
          _ -> world
        nxtScene = case L.listSelected $ options world of
          Just 0 -> HeroInfo
          Just 1 -> Dungeons
          _ -> Main

handleDungeonScene :: World -> Input -> World
handleDungeonScene world@World{dungeonsPage = d_page} inp = nw
  where nw = case inp of
          CharKey 'm' -> world{currentScene = Main}
          KeyUP -> world{dungeonsPage= selectUp d_page}
          KeyDown -> world{dungeonsPage= selectDown d_page}
          Enter -> case state dg of
            NoMission -> world{currentScene = DungeonPrepare, dungeonPrep = defaultPrepPage hs dg}
            MissionComplete -> world
            -- MissionComplete -> world{currentScene = FightResultScene,
            --                          wealth = wealth world + money battleResult,
            --                          heros = updatedHs,
            --                          dungeonsPage = resetCompletedDungeon d_page,
            --                          battleResultPage = BattleResultPage battleResult }
            InProgress -> world
          _ -> world
        (_,dg) = fromJust $ L.listSelectedElement $ dungeons d_page
        -- battleResult = calcBattle dg $ randomGen world
        -- updatedHs = fmap restoreHealth $ (heros world) ++ herosComeBack
        -- herosComeBack = updatedHero battleResult
        -- restoreHealth h = h{hp = maxHP h}
        hs = heros world

handleHeroInfoScene :: World -> Input -> World
handleHeroInfoScene world inp = nw
  where nw = case inp of
          CharKey 'm' -> world{currentScene = Main}
          _ -> world

handleDungeonPrepareScene :: World -> Input -> World
handleDungeonPrepareScene world@World{dungeonPrep = d_prep, dungeonsPage = d_page} inp = nw
  where
    newPrep n = addOrRemoveHero d_prep
    nw = case inp of
      CharKey 'd' -> world{currentScene = Dungeons,
                           dungeonPrep = d_prep{team = L.list Normal (Vec.fromList []) 1}}
      KeyRight -> world{dungeonPrep = addMode d_prep}
      KeyLeft -> world{dungeonPrep = removeMode d_prep}
      CharKey 's' -> if ((length $ team d_prep) > 0)
           then world{currentScene = Dungeons
                     , heros = removeAllEqualElms (heros world) (team d_prep)
                     , dungeonsPage = comeBackFromStartMission d_page (team d_prep)}
           else world
      (NumKey n) -> world{dungeonPrep = newPrep (n-1)}
      _ -> world

handleFightResultScene :: World -> Input -> World
handleFightResultScene world@World{battleResultPage = brp} inp = nw
  where nw = case inp of
          CharKey 'm' -> world{currentScene = Main}
          CharKey 'd' -> world{currentScene = Dungeons}

gameTick :: World -> World
gameTick w@World{dungeonsPage = dp} = w{dungeonsPage = n_dp}
  where n_dp = dungeonsPageTick dp
