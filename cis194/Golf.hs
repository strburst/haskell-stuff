module Golf where

-- skips :: [a] -> [[a]]

nthElemList :: Int -> [a] -> [a]
nthElemList n [] = []
nthElemList n xs = ((drop xs) !! n) : nthElemList n (drop n xs)
