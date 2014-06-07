module Golf where

-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips xs = map (everyNth xs) [1..(length xs)]

everyNth :: [a] -> Int -> [a]
everyNth xs n = map snd $ filter (\(i, _) -> (i + 1) `mod` n == 0) (zip [0..] xs)

