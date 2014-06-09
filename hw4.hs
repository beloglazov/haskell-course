-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> if x then acc + 1 else acc) (0 :: Integer)

-- map' odd [1, 2, 3, 4] == [True, False, True, False]
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
