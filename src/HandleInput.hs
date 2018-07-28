module HandleInput where

import Core.World
import Core.DungeonPrepPage
import Core.DungeonsPage
import Core.Dungeon(DungeonState(..), Dungeon(..), BattleResult(..))
import Core.BattleResultPage
import Input

handleMainScene :: World -> Input -> World
handleMainScene world inp = nw
  where nw = case inp of
          D -> world{currentScene = Dungeons}
          H -> world{currentScene = HeroInfo}

handleDungeonScene :: World -> Input -> World
handleDungeonScene world@World{dungeonsPage = d_page, dungeonPrep = d_prep} inp = nw
  where nw = case inp of
          M -> world{currentScene = Main}
          J -> world{dungeonsPage = selectDown d_page}
          K -> world{dungeonsPage = selectUp d_page}
          Enter -> case state dg of
            NoMission -> world{currentScene = DungeonPrepare, dungeonPrep = defaultPrepPage hs dg}
            MissionComplete -> world{currentScene = FightResultScene,
                                     dungeonsPage = resetCompletedDungeon d_page,
                                     battleResultPage = BattleResultPage BattleResult{
                                        money = 1,
                                        updatedHero = []}
                                    }
            _ -> world
          _ -> world
        dg = getSelectedDungeon d_page
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
                     , dungeonsPage = comeBackFromStartMission d_page}
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
