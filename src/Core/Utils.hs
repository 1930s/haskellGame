module Core.Utils (removeAt) where

removeAt :: [a] -> Int -> Maybe [a]
removeAt lst idx = if idx >= 0 && idx < length lst
                   then Just $ take idx lst ++ drop (idx+1) lst
                   else Nothing
