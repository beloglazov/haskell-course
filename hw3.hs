module Golf where

-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips xs = map (everyNth xs) [1..(length xs)]

everyNth :: [a] -> Int -> [a]
everyNth xs n = map snd $ filter (\(i, _) -> i `mod` n == 0) (zip [1..] xs)

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(x, _, _) -> x) $
    filter
        (\(x1, x2, x3) -> x1 > x2 && x1 > x3)
        (zip3 (tail xs) xs (drop 2 xs))

-- putStr $ histogram [1,1,1,5]
-- histogram [3,5] == "   * *    \n==========\n0123456789\n"
histogram :: [Integer] -> String
histogram xs =
    let freq = map (\x -> length $ filter (x ==) xs) [0..9] in
    fst (foldr
        (\_ (res, x) -> (showLevel x ++ "\n" ++ res, map (subtract 1) x))
        ("", freq)
        [1..maximum freq])
    ++ "==========\n0123456789\n"

showLevel :: [Int] -> String
showLevel = map (\x -> if x > 0 then '*' else ' ')

