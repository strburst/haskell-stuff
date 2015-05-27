import Data.Char

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = map (toInteger) (map (digitToInt) (show x))

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

mapEveryOtherLeft :: (a -> a) -> [a] -> [a]
mapEveryOtherLeft f []       = []
mapEveryOtherLeft f [x]      = [f x]
mapEveryOtherLeft f (x:y:zs) = (f x) : y : mapEveryOtherLeft f zs

doubleEveryOther :: Num a => [a] -> [a]
doubleEveryOther xs = reverse (mapEveryOtherLeft (*2) (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits [x]    = sum (toDigits x)
sumDigits (x:ys) = sum (toDigits x) + sumDigits ys

flatten :: [[a]] -> [a]
flatten []      = []
flatten [xs]    = xs
flatten (xs:ys) = xs ++ flatten ys

singleDigitList xs = flatten (map (toDigits) xs)

validate x = (sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10) == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disks peg1 peg2 peg3
    | disks <= 1 = [(peg1, peg3)]
    | otherwise  = (hanoi (disks - 1) peg1 peg3 peg2)
        ++ ((peg1, peg3) : (hanoi (disks - 1) peg2 peg1 peg3))

hanoiMoveLength disks = disks ^ 2 - 1

