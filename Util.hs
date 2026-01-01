module Util where
getValue :: Eq a => a -> [(a, b)] -> b
getValue key dict = snd $ head $ filter (\x -> fst x == key) dict

-- >>> getValue 'a' [('a', 'b'), ('b', 'c'), ('c', 'd')]
-- 'b'

