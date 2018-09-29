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
                            moveUpDownselection,
                            DungeonPrepPage(..))
import Core.DungeonsPage
import Core.Dungeon(DungeonState(..),
                    Dungeon(..),
                    BattleResult(..),
                    calcBattle)
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
    Fight -> handleBattleScene w i

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
            MissionComplete -> world{currentScene = FightResultScene,
                                     wealth = wealth world + money battleResult,
                                     heros = updatedHs,
                                     dungeonsPage = resetCompletedDungeon d_page,
                                     battleResultPage = BattleResultPage battleResult }
            InProgress -> world
          _ -> world
        (_,dg) = fromJust $ L.listSelectedElement $ dungeons d_page
        battleResult = calcBattle dg $ randomGen world
        updatedHs = fmap restoreHealth $ foldr (\h l-> L.listInsert 0 h l) (heros world) herosComeBack
        herosComeBack = updatedHero battleResult
        restoreHealth h = h{hp = maxHP h}
        hs = heros world

handleHeroInfoScene :: World -> Input -> World
handleHeroInfoScene world inp = nw
  where nw = case inp of
          CharKey 'm' -> world{currentScene = Main}
          KeyUp -> world{heros = L.listMoveUp (heros world)}
          KeyDown -> world{heros = L.listMoveDown (heros world)}
          _ -> world

handleDungeonPrepareScene :: World -> Input -> World
handleDungeonPrepareScene world@World{dungeonPrep = d_prep} inp = nw
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
                     then world{currentScene = Fight,
                                battlePage = Just $ BP.initialiseBattlePage
                                             heroList
                                             enemyList
                                             (randomGen world)
                                             20
                               }
                     -- then world{currentScene = Dungeons
                     --           , heros = removeAllEqualElms (heros world) (team d_prep)
                     --           , dungeonsPage = comeBackFromStartMission d_page (team d_prep)}
                     else world
        where enemyList = L.list BattleEnemies
                          (Vec.fromList $ Core.Dungeon.enemies $ dungeon d_prep)
                          1
              heroList = L.list BattleHeros (L.listElements $ team d_prep) 1
      Enter -> world{dungeonPrep = newPrep }
      _ -> world

handleFightResultScene :: World -> Input -> World
handleFightResultScene world inp = nw
  where nw = case inp of
          CharKey 'm' -> world{currentScene = Main}
          CharKey 'd' -> world{currentScene = Dungeons}
          _ -> world

handleBattleScene :: World -> Input -> World
handleBattleScene w@World{battlePage = Nothing} _ = w
handleBattleScene w@World{battlePage = Just bp} inp = nw
  where nw = case inp of
          KeyUp -> w{battlePage = Just $ BP.handleSelectUp bp}
          KeyDown -> w{battlePage = Just $ BP.handleSelectDown bp}
          Enter -> w{battlePage = Just $ BP.handleConfirmAction bp}
          _ -> w

gameTick :: World -> World
gameTick w@World{battlePage = bp} = w{battlePage= Just n_bp}
  where n_bp = BP.handleGameTick $ fromJust bp
