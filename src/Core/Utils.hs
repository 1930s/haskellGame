module Core.Utils (removeAt, modifyAt, replaceAt) where

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
