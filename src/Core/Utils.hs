module Core.Utils (removeAt, modifyAt) where

isIndexValid :: [a] -> Int -> Bool
isIndexValid lst idx = idx >= 0 && idx < length lst

removeAt :: [a] -> Int -> Maybe [a]
removeAt lst idx = if isIndexValid lst idx
                   then Just $ take idx lst ++ drop (idx+1) lst
                   else Nothing

modifyAt :: [a] -> (a -> a) -> Int -> Maybe [a]
modifyAt lst func idx = if isIndexValid lst idx
                        then Just $ take idx lst ++ [func (lst !! idx)] ++ drop (idx+1) lst
                        else Nothing
