-- Intermediate Haskell/Other data structures
-- https://en.wikibooks.org/wiki/Haskell/Other_data_structures

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

tree = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x)     = Leaf (f x)
mapTree f (Branch x y) = Branch (mapTree f x) (mapTree f y)

foldTree :: (b -> a -> b) -> b -> Tree a -> b
foldTree f i (Leaf x)     = f i x
foldTree f i (Branch left right) = foldTree f (foldTree f i left) right

data BinTree a = BinEmpty
               | BinBranch a (BinTree a) (BinTree a)
               deriving (Show)

binTree1 = BinBranch 2
    (BinBranch 1 BinEmpty BinEmpty)
    (BinBranch 3 BinEmpty BinEmpty)

binTree2 = BinBranch 10
    (BinBranch 5
        (BinBranch 4 BinEmpty BinEmpty)
        (BinBranch 6 BinEmpty BinEmpty))
    (BinBranch 15
        (BinBranch 14 BinEmpty BinEmpty)
        BinEmpty)

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree f BinEmpty                 = BinEmpty
mapBinTree f (BinBranch x left right) = BinBranch (f x) (mapBinTree f left) (mapBinTree f right)

preorder :: BinTree a -> [a]
preorder BinEmpty                    = []
preorder (BinBranch elem left right) = elem : ((preorder left) ++ (preorder right))

inorder :: BinTree a -> [a]
inorder BinEmpty                    = []
inorder (BinBranch elem left right) = (inorder left) ++ [elem] ++ (inorder right)

postorder :: BinTree a -> [a]
postorder BinEmpty                    = []
postorder (BinBranch elem left right) = (postorder left) ++ (postorder right) ++ [elem]

everyB :: (a -> Bool) -> BinTree a -> Bool
everyB f BinEmpty                    = True
everyB f (BinBranch elem left right) = (f elem) && everyB f left && everyB f right

isBST :: Ord a => BinTree a -> Bool
isBST BinEmpty = True
isBST (BinBranch elem left right) = leftLess && rightGreater && isBST left && isBST right
    where leftLess = case left of
              (BinBranch leftElem _ _) -> elem > leftElem
              BinEmpty                 -> True
          rightGreater = case right of
              (BinBranch rightElem _ _) -> elem < rightElem
              BinEmpty                  -> True

data Weird a b = First a
               | Second b
               | Third [(a,b)]
               | Fourth (Weird a b)
               deriving (Show)

weirdthing = Fourth $ Third [('a', 5), ('b', 7)]

mapWeird1 :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
mapWeird1 f g weird = case weird of
    First item    -> First (f item)
    Second item   -> Second (g item)
    Third list    -> Third (mapfgList list)
    Fourth weird  -> Fourth (mapWeird1 f g weird)
    where mapfgList [] = []
          mapfgList ((itemA, itemB):etc) = (f itemA, g itemB) : mapfgList etc

mapWeird2 :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
mapWeird2 f g weird = case weird of
    First item    -> First (f item)
    Second item   -> Second (g item)
    Third list    -> Third (zip (map f listA) (map g listB))
        where listA = fst (unzip list); listB = snd (unzip list)
    Fourth weird  -> Fourth (mapWeird2 f g weird)

mapWeird3 :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
mapWeird3 f g weird = case weird of
    First item    -> First (f item)
    Second item   -> Second (g item)
    Third ((itemA, itemB):etc) -> Third ((f itemA, g itemB) : ((\(Third list) -> list) (mapWeird3 f g (Third etc))))
    Third [] -> Third []
    Fourth weird  -> Fourth (mapWeird3 f g weird)