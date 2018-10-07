module HandleInput where

import qualified Data.Vector as Vec
import Data.Maybe
import Core.Utils(CursorName(..))
import Core.World
import Core.DungeonPrepPage(addOrRemoveHero,
                            defaultPrepPage,
                            addMode,
                            removeMode,
                            moveUpDownselection,
                            DungeonPrepPage(..))
import Core.DungeonsPage
import Core.Dungeon(DungeonState(..),
                    Dungeon(..),
                    BattleResult(..))
import qualified Core.BattlePage as BP
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
    FightScene -> handleBattleScene w i

handleMainScene :: World -> Input -> World
handleMainScene world inp = nw
  where nw = case inp of
          KeyUp -> world{options = L.listMoveUp $ options world}
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
          KeyUp -> world{dungeonsPage= selectUp d_page}
          KeyDown -> world{dungeonsPage= selectDown d_page}
          Enter -> case state dg of
            NoMission -> world{currentScene = DungeonPrepare, dungeonPrep = defaultPrepPage hs dg}
            _ -> world
          _ -> world
        (_,dg) = fromJust $ L.listSelectedElement $ dungeons d_page
        hs = heros world

handleHeroInfoScene :: World -> Input -> World
handleHeroInfoScene world inp = nw
  where nw = case inp of
          CharKey 'm' -> world{currentScene = Main}
          KeyUp -> world{heros = L.listMoveUp (heros world)}
          KeyDown -> world{heros = L.listMoveDown (heros world)}
          _ -> world

handleDungeonPrepareScene :: World -> Input -> World
handleDungeonPrepareScene world@World{
  dungeonPrep = d_prep,
  heros = hs
  } inp = nw
  where
    newPrep = addOrRemoveHero d_prep
    nw = case inp of
      CharKey 'd' -> world{currentScene = Dungeons,
                           dungeonPrep = d_prep{team = L.list Normal (Vec.fromList []) 1}}
      KeyRight -> world{dungeonPrep = addMode d_prep}
      KeyLeft -> world{dungeonPrep = removeMode d_prep}
      KeyUp -> world{dungeonPrep = moveUpDownselection d_prep KeyUp}
      KeyDown -> world{dungeonPrep = moveUpDownselection d_prep KeyDown}
      CharKey 's' -> if ((length $ team d_prep) > 0)
                     then world{currentScene = FightScene,
                                heros = removeAllEqualElms hs $ team d_prep,
                                battlePage = Just $ BP.initialiseBattlePage
                                             heroList
                                             enemyList
                                             (randomGen world)
                                             5
                               }
                     else world
        where enemyList = L.list BattleEnemies
                          (Vec.fromList $ Core.Dungeon.enemies $ dungeon d_prep)
                          1
              heroList = L.list BattleHeros (L.listElements $ team d_prep) 1
      Enter -> world{dungeonPrep = newPrep }
      _ -> world

handleFightResultScene :: World -> Input -> World
handleFightResultScene world@World{
  heros = hs,
  battleResultPage = BattleResultPage{result = BattleResult{updatedHero = updatedHs}}
  } inp = nw
  where nw = case inp of
          CharKey 'm' -> worldWithHerosBack{currentScene = Main}
          CharKey 'd' -> worldWithHerosBack{currentScene = Dungeons}
          _ -> world
        worldWithHerosBack = world{
          heros = L.list Normal (L.listElements hs Vec.++ L.listElements updatedHs) 1
          }

handleBattleScene :: World -> Input -> World
handleBattleScene w@World{battlePage = Nothing} _ = w
handleBattleScene w@World{battlePage = Just bp} inp = nw
  where nw = case inp of
          KeyUp -> w{battlePage = Just $ BP.handleSelectUp bp}
          KeyDown -> w{battlePage = Just $ BP.handleSelectDown bp}
          Enter -> w{battlePage = Just $ BP.handleConfirmAction bp}
          _ -> w

handleFightOver :: World -> World
handleFightOver w@World{
  battlePage = Just bp,
  wealth = wel,
  currentScene = FightScene
  } = case BP.isFightOver bp of
        True -> w{currentScene = FightResultScene,
                  wealth = wel + BP.totalReward bp,
                  battleResultPage = BP.generateBattleResult bp}
        _ -> w
handleFightOver w = w

gameTick :: World -> World
gameTick w@World{
  currentScene = FightScene,
  battlePage = Just bp} = nw
  where nw = handleFightOver w{battlePage= Just n_bp}
        n_bp = BP.handleGameTick bp

gameTick w = w
