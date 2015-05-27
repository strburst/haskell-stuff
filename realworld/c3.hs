import Data.List
import Data.Char

data BookInfo = Book Int String [String]
                deriving (Show)

type CustomerID = Integer
type Address    = [String]

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

myLength :: Num b => [a] -> b
myLength (_:xs) = 1 + myLength xs
myLength []     = 0

threeTwoList (3:2:xs) = True
threeTwoList _        = False

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

fromListCon (Cons x y)   = x : fromListCon y
fromListCon Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data MaybeTree a = MBNode a (Maybe (MaybeTree a)) (Maybe (MaybeTree a))
                   deriving (Show)

treeHt Empty               = 0
treeHt (Node _ left right) = 1 + max (treeHt left) (treeHt right)

avgList xs = sum xs / fromIntegral (length xs)
avgListDiv xs = sum xs `div` length xs

palindromeList xs = xs ++ reverse xs

-- isPal :: Eq a => [a] -> Bool
-- isPal xs = take halfLength xs == reverse (drop halfLength xs)
--     where halfLength = length xs `div` 2

-- isPal :: Eq a => [a] -> Bool
isPal xs = xs == reverse xs

subSort xs = sortBy (compareLength) xs
    where compareLength xs ys = compare (length xs) (length ys)

strSumSort :: [String] -> [String]
strSumSort xs = sortBy (compareStrSum) xs
    where compareStrSum xs ys = compare (sumLength xs) (sumLength ys)
          sumLength xs = sum (map (ord) xs)

joinSimple [] sep = []
joinSimple xs sep = head xs ++ sep ++ (joinSimple (tail xs) sep)

printList []     = print "empty"
printList (x:xs) = print xs

join []     sep = []
join (x:[]) sep = x
join (x:xs) sep = x ++ sep ++ join xs sep

-- join2 xs sep
--     | length xs <= 1 = xs
--     | otherwise      = head xs ++ sep ++ (join2 (tail xs) sep)
--

cross :: [a] -> [b] -> [(a,b)]
cross xs ys = [ (x, y) | x <- xs, y <- ys ]
