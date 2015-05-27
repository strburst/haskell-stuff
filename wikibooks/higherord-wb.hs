import Data.Char

quicksort :: (a -> a -> Ordering) -> [a] -> [a]
quicksort compare (p:xs) = (quicksort compare lesser) ++ (p : equal) ++ (quicksort compare greater)
    where lesser  = filter (\ e -> e `compare` p == LT) xs
          equal   = filter (\ e -> e `compare` p == EQ) xs
          greater = filter (\ e -> e `compare` p == GT) xs

quicksort _ [] = []

quicksortP :: (a -> a -> Ordering) -> [a] -> [a]
quicksortP _ [] = []

quicksortP compare xs = (quicksortP compare lesser) ++ (equal) ++ (quicksortP compare greater)
    where lesser  = filter (\ e -> e `compare` pivot == LT) xs
          equal   = filter (\ e -> e `compare` pivot == EQ) xs
          greater = filter (\ e -> e `compare` pivot == GT) xs
          pivot   = middle compare (head xs) (xs !! ((length xs) `quot` 2)) (last xs)

middle :: (a -> a -> Ordering) -> a -> a -> a -> a
middle compare x y z
    | (x `compare` y /= LT) && (y `compare` z /= LT) = y   -- x >= y >= z
    | (x `compare` y /= GT) && (y `compare` z /= GT) = y   -- x <= y <= z
    | otherwise                                      = middle compare z x y

-- quickselect :: Integral b => (a -> a -> Ordering) -> b ->  [a] -> [a]
-- quickselect compare (p:xs) =

insensitive :: [String] -> [String]
insensitive xs = quicksort (\a b -> compare (map toLower a) (map toLower b)) xs
