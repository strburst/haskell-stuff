module BinTree (
    add,
    makeTree,
    addAll,
    preorder,
    inorder,
    postorder,
    minOf,
    maxOf,
    remove
    ) where

-- A typical binary tree, holding data and left/right branches
data BinTree a = BinEmpty
               | BinBranch a (BinTree a) (BinTree a)
               deriving (Eq, Show)

add :: (Ord a) => a -> BinTree a -> BinTree a
add item (BinBranch root left right)
    | item < root = BinBranch root (add item left) right
    | item > root = BinBranch root left (add item right)
    | otherwise   = BinBranch root left right
add item BinEmpty = BinBranch item BinEmpty BinEmpty

makeTree :: (Ord a, Foldable t) => t a -> BinTree a
makeTree = foldl (flip add) BinEmpty

addAll :: (Ord a, Foldable t) => t a-> BinTree a -> BinTree a
addAll = flip $ foldl (flip add)

-- Compute the preorder traversal of a given binary tree
preorder :: BinTree a -> [a]
preorder BinEmpty                    = []
preorder (BinBranch elem left right) = elem : ((preorder left) ++ (preorder right))

-- Compute the inorder traversal of a given binary tree
inorder :: BinTree a -> [a]
inorder BinEmpty                    = []
inorder (BinBranch elem left right) = (inorder left) ++ [elem] ++ (inorder right)

-- Compute the postorder traversal of a given binary tree
postorder :: BinTree a -> [a]
postorder BinEmpty                    = []
postorder (BinBranch elem left right) = (postorder left) ++ (postorder right) ++ [elem]

-- Get the smallest element in a binary tree
minOf :: BinTree a -> a
minOf (BinBranch elem BinEmpty _) = elem
minOf (BinBranch _ left _) = minOf left
minOf BinEmpty = error "Empty tree lacks a minOf"

-- Get the largest element in a binary tree
maxOf :: BinTree a -> a
maxOf (BinBranch elem _ BinEmpty) = elem
maxOf (BinBranch _ _ right) = maxOf right
maxOf BinEmpty = error "Empty tree lacks a maxOf"

remove :: (Ord a) => a -> BinTree a -> BinTree a
remove _ BinEmpty = BinEmpty
remove item (BinBranch elem left right)
    | item < elem = BinBranch elem (remove item left) right
    | item > elem = BinBranch elem left (remove item right)
    | left == BinEmpty = right
    | right == BinEmpty = left
    | otherwise = BinBranch (successor) left (remove successor right)
        where successor = minOf right
