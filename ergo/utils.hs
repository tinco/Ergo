module Ergo.Utils where

-- Updates a list by setting the n-th value to a
update :: [a] -> Int -> a -> [a]
update list n a = (take n list) ++ [a] ++ (drop (n+1) list)
