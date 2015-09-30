import qualified Data.Map as Map

docdiff :: String -> String -> Integer
docdiff a b = 0

-- |Compute the dot product of two lists of numbers
dot :: Num a => [a] -> [a] -> a
dot a b = sum $ zipWith (*) a b

-- mapFreq ::

-- |Count the occurrences of each word in the given string
wordCount :: String -> Map.Map String Integer
wordCount = count . words

-- |Count the number of times each item in a list
count :: (Ord k) => [k] -> Map.Map k Integer
count = foldr (incCount) Map.empty

-- |Increment the given key's value
incCount :: (Ord k, Num a) => k -> Map.Map k a -> Map.Map k a
incCount k m = case prev of
    Just count -> Map.insert k (count + 1) m
    Nothing    -> Map.insert k 1 m
    where prev = Map.lookup k m
