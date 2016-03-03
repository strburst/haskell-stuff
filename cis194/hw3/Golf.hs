module Golf (skips, localMaxima, histogram) where

import Data.List (intercalate)

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

histogram :: [Integer] -> String
histogram xs = intercalate "\n" (lines ++ [eqLine, numLine])
  where lines = map (\n -> testToStr (>=n) '*' ' ' count) (reverse [1..mode])
        mode = snd $ foldr1
          (\a@(_, n1) b@(_, n2) -> if n1 >= n2 then a else b)
          (tupIndex $ count)
        count = foldr (changeNth (+1)) (replicate 10 0) xs
        eqLine = replicate 10 '='
        numLine = showCat [0..9]

tupIndex :: (Num a) => [t] -> [(a, t)]
tupIndex = countMap 0
  where countMap n (x:xs) = (n, x) : countMap (n + 1) xs
        countMap _ []     = []

changeNth :: (Integral n) => (a -> a) -> n -> [a] -> [a]
changeNth f 0 (x:xs) = (f x) : xs
changeNth f n (x:xs) = x : (changeNth f (n - 1) xs)
changeNth _ _ []     = error "No elements left"

testToStr :: (a -> Bool) -> Char -> Char -> [a] -> [Char]
testToStr pred ifYes ifNo xs = map decide xs
  where decide x = if (pred x) then ifYes else ifNo

showCat :: (Show a) => [a] -> [Char]
showCat xs = foldr cat "" xs
  where cat x acc = (show x) ++ acc
