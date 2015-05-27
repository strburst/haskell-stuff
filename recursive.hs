myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:[]) = x:[]
myReverse (x:xs) = myReverse xs ++ [x]

mySum :: Num a => [a] -> a
mySum (x:xs) = x + mySum xs
mySum []     = 0

myAnd :: [Bool] -> Bool
myAnd (b:bs) = b && myAnd bs
myAnd []     = True

-- myAnd2 :: [Bool] -> Bool
-- myAnd2 xs = foldr (\
