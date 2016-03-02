module Golf where

skips :: [a] -> [[a]]
skips base = map ((flip everyNth) base) [1..(length base)]

everyNth :: Int -> [a] -> [a]
everyNth n xs =
  if (length xs) < n
    then []
    else (xs !! (n - 1)) : everyNth n (drop n xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map mid $ filter lmax $ tupGroup3 xs
  where mid (x, y, z) = y
        lmax (x, y, z) = x < y && z < y

tupGroup3 :: [a] -> [(a, a, a)]
tupGroup3 (x:y:z:xs) = (x, y, z) : (tupGroup3 (y:z:xs))
tupGroup3 _          = []
