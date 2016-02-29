import qualified Data.Map as Map

-- |Compute the "distance" of one string from another
docdiff :: (Ord a, Fractional a) => String -> String -> a
docdiff a b = (dot docVecA docVecB) / (max (mag2 docVecA) (mag2 docVecB))
    where docVecA = mapFreq a
          docVecB = mapFreq b

-- |Compute the squared magnitude of a list of numbers
mag2 :: Num a => [a] -> a
mag2 a = dot a a

-- |Compute the dot product of two lists of numbers
dot :: Num a => [a] -> [a] -> a
dot a b = sum $ zipWith (*) a b

-- |Replace the words in a string with a list of frequencies corresponding to
-- each word
mapFreq :: (Num a) => String -> [a]
mapFreq string = map getCount $ words string
    where counts        = wordCount string
          getCount word = Map.findWithDefault 0 word counts

-- |Count the occurrences of each word in the given string
wordCount :: (Num a) => String -> Map.Map String a
wordCount = count . words

-- |Count the number of times each item in a list
count :: (Ord k, Num a) => [k] -> Map.Map k a
count = foldr incCount Map.empty

-- |Increment the given key's value
incCount :: (Ord k, Num a) => k -> Map.Map k a -> Map.Map k a
incCount k m = case prev of
    Just count -> Map.insert k (count + 1) m
    Nothing    -> Map.insert k 1 m
    where prev = Map.lookup k m
