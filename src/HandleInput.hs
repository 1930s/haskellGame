module HandleInput where

import Core.World
import Core.DungeonPrepPage
import Core.DungeonsPage(selectDown, selectUp, getSelectedDungeon)
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
          Enter -> world{currentScene = DungeonPrepare, dungeonPrep = d_prep{dungeon = dg}}
          _ -> world
        dg = getSelectedDungeon d_page

handleHeroInfoScene :: World -> Input -> World
handleHeroInfoScene world inp = nw
  where nw = case inp of
          M -> world{currentScene = Main}
          _ -> world

handleDungeonPrepareScene :: World -> Input -> World
handleDungeonPrepareScene world@World{dungeonPrep = d_prep} inp = nw
  where
    newPrep n = addOrRemoveHero n d_prep
    nw = case inp of
      D -> world{currentScene = Dungeons, dungeonPrep = d_prep{team = []}}
      A -> world{dungeonPrep = addMode d_prep}
      R -> world{dungeonPrep = removeMode d_prep}
      (Input n) -> world{dungeonPrep = newPrep (n-1)}

