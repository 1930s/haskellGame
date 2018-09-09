module HandleInput where

import Data.List
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
                    calcBattle)
import Core.BattleResultPage
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
          D -> world{currentScene = Dungeons}
          H -> world{currentScene = HeroInfo}

handleDungeonScene :: World -> Input -> World
handleDungeonScene world@World{dungeonsPage = d_page} inp = nw
  where nw = case inp of
          M -> world{currentScene = Main}
          J -> world{dungeonsPage = selectDown d_page}
          K -> world{dungeonsPage = selectUp d_page}
          Enter -> case state dg of
            NoMission -> world{currentScene = DungeonPrepare, dungeonPrep = defaultPrepPage hs dg}
            MissionComplete -> world{currentScene = FightResultScene,
                                     wealth = wealth world + money battleResult,
                                     heros = updatedHs,
                                     dungeonsPage = resetCompletedDungeon d_page,
                                     battleResultPage = BattleResultPage battleResult }
            InProgress -> world
          _ -> world
        dg = getSelectedDungeon d_page
        battleResult = calcBattle dg $ randomGen world
        updatedHs = fmap restoreHealth $ (heros world) ++ herosComeBack
        herosComeBack = updatedHero battleResult
        restoreHealth h = h{hp = maxHP h}
        hs = heros world

handleHeroInfoScene :: World -> Input -> World
handleHeroInfoScene world inp = nw
  where nw = case inp of
          M -> world{currentScene = Main}
          _ -> world

handleDungeonPrepareScene :: World -> Input -> World
handleDungeonPrepareScene world@World{dungeonPrep = d_prep, dungeonsPage = d_page} inp = nw
  where
    newPrep n = addOrRemoveHero n d_prep
    nw = case inp of
      D -> world{currentScene = Dungeons, dungeonPrep = d_prep{team = []}}
      A -> world{dungeonPrep = addMode d_prep}
      R -> world{dungeonPrep = removeMode d_prep}
      S -> if ((length $ team d_prep) > 0)
           then world{currentScene = Dungeons
                     , heros = heros world \\ (team d_prep)
                     , dungeonsPage = comeBackFromStartMission d_page (team d_prep)}
           else world
      (Input n) -> world{dungeonPrep = newPrep (n-1)}

handleFightResultScene :: World -> Input -> World
handleFightResultScene world@World{battleResultPage = brp} inp = nw
  where nw = case inp of
               M -> world{currentScene = Main}
               D -> world{currentScene = Dungeons}

gameTick :: World -> World
gameTick w@World{dungeonsPage = dp} = w{dungeonsPage = n_dp}
  where n_dp = dungeonsPageTick dp
