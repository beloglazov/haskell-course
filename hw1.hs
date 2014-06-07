-- toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x > 0     = x `mod` 10 : toDigitsRev (x `div` 10)
    | otherwise = []

-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []         = []
doubleEveryOtherRev (x:[])     = [x]
doubleEveryOtherRev (x:y:rest) = x : (y * 2) : doubleEveryOtherRev rest

-- sumDigits [16,7,12,5] == 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)

-- validate 4012888888881881 == True
-- validate 4012888888881882 == False
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
type Peg  = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi 1 p1 p2 _  = [(p1, p2)]
hanoi n p1 p2 p3 =
    hanoi (n - 1) p1 p3 p2 ++
    hanoi 1 p1 p2 p3 ++
    hanoi (n - 1) p3 p2 p1

