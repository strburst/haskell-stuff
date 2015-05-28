-- Elementary Haskell/List processing
-- https://en.wikibooks.org/wiki/Haskell/List_processing

-- Recursive definition of and
andR :: [Bool] -> Bool
-- andR (x:xs) = if x then andR xs else False
andR (x:xs)
    | x         = andR xs
    | otherwise = False
andR []     = True

-- Fold definition of and
andF :: [Bool] -> Bool
-- andF xs = foldr (&&) True xs
andF = foldr (&&) True              -- point free style!

-- Not sure what this does
hmm xs = foldr (id) True xs

-- Recursive definition of or
orR :: [Bool] -> Bool
orR (x:xs) =
    if x
        then True
        else
            orR xs
orR [] = False

-- Fold definition of or
orF :: [Bool] -> Bool
orF = foldr (||) False

-- Evaluate to true if all elements in xs are distinct
distinct :: Eq a => [a] -> Bool
distinct (x:xs) = and (map (x/=) xs) && distinct xs
distinct [] = True

-- Filter xs by whether each element is divisible by n
returnDivisible :: Int -> [Int] -> [Int]
returnDivisible n xs = [ x | x <- xs, x `mod` n == 0]

-- For each list in a list of lists, include the tail of the list if the first
-- element is greater than 5
tailHeadGT5 :: [[Int]] -> [[Int]]
tailHeadGT5 listxs = [ tail xs | xs <- listxs, head xs > 5 ]

-- Recursive definition of scan
scanlR :: (b -> a -> b) -> b -> [a] -> [b]
scanlR f i (x:xs) = i : scanlR f (f i x) xs
scanlR _ i [] = [i]

-- scanlF :: (b -> a -> b) -> b -> [a] -> [b]
-- scanlF f i xs =

-- Compute a list of the 1..nth factorial
factList :: Integer -> [Integer]
factList n = scanl1 (*) [1..n]

