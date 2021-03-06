module Core.Utils (removeAt,
                   modifyAt,
                   replaceAt,
                   removeAllEqualElms,
                   listReplaceAt,
                   CursorName(..)
                   ) where

import Data.Maybe
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

data CursorName = Normal
                | DungeonPrepareTeam
                | DungeonPrepareBench
                | BattleHeros
                | BattleEnemies
                | BattleResultEquipment
                deriving(Eq, Ord, Show)

isIndexValid :: [a] -> Int -> Bool
isIndexValid lst idx = idx >= 0 && idx < length lst

removeAt :: [a] -> Int -> Maybe [a]
removeAt lst idx = if isIndexValid lst idx
                   then Just $ take idx lst ++ drop (idx+1) lst
                   else Nothing

replaceAt :: [a] -> a -> Int -> Maybe [a]
replaceAt ls r = modifyAt ls (\_ -> r)

modifyAt :: [a] -> (a -> a) -> Int -> Maybe [a]
modifyAt lst func idx = if isIndexValid lst idx
                        then Just $ take idx lst ++ [func (lst !! idx)] ++ drop (idx+1) lst
                        else Nothing

removeAllEqualElms :: Eq n => L.List CursorName n -> L.List CursorName n -> L.List CursorName n
removeAllEqualElms lst rmv = foldr (\i l -> L.listRemove i l) lst idxes
  where lstVec = L.listElements lst
        rmvVec = L.listElements rmv
        idxes = [ getIdxToRmv i | i <- [0..(length rmvVec -1)]]
        getIdxToRmv n = fromJust $ Vec.findIndex (\e -> e == rmvVec Vec.! n) lstVec

listReplaceAt :: Int -> n -> L.List CursorName n -> L.List CursorName n
listReplaceAt idx e lst = L.listModify (\ _ -> e) $ L.listMoveTo idx lst
