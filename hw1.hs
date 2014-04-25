toDigitsRev :: Integer -> [Integer]
toDigitsRev x
        | x > 0     = x `mod` 10 : toDigitsRev (x `div` 10)
        | otherwise = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:rest) = x : (y * 2) : doubleEveryOtherRev rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 = undefined
