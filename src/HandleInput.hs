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
    allHeros = heros world
    currentTeam = team d_prep
    selected n = allHeros !! n
    selectValid n = n < length allHeros && (not $ (selected n) `elem` currentTeam)
    newPrep n = if selectValid n
                then d_prep{team = (heros world !! n) : (team d_prep)}
                else d_prep
    nw = case inp of
      D -> world{currentScene = Dungeons, dungeonPrep = d_prep{team = []}}
      (Input n) -> world{dungeonPrep = newPrep (n-1)}

