{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Tree.Util
  (
    lookupTree
  , lookupTreeInForest
  , find
  , findSubTreeInForest
  , filter
  , filterSub
  , size
  , mirror
  ) where

import Prelude hiding (filter)
import Data.Tree
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Monoid

instance (Ord a) => Ord (Tree a)

-- | get the sub-tree rooted at the first (left-most, depth-first) occurrence
-- of the specified node value
lookupTree :: Eq a => a -> Tree a -> Maybe (Tree a)
lookupTree v t
    | rootLabel t == v = Just t
    | otherwise = lookupTreeInForest v $ subForest t

-- | get the sub-tree for the specified node value in the first tree in
-- forest in which it occurs.
lookupTreeInForest :: Eq a => a -> [Tree a] -> Maybe (Tree a)
lookupTreeInForest _ [] = Nothing
lookupTreeInForest v (t:ts) = case lookupTree v t of
                             Just t' -> Just t'
                             Nothing -> lookupTreeInForest v ts

find :: (a -> Bool) -> Tree a -> Maybe (Tree a)
find f t
  | f (rootLabel t) = Just t
  | otherwise       = findSubTreeInForest f $ subForest t

findSubTreeInForest :: (a -> Bool) -> [Tree a] -> Maybe (Tree a)
findSubTreeInForest _ [] = Nothing
findSubTreeInForest f (t:ts) = case find f t of
                             Just t' -> Just t'
                             Nothing -> findSubTreeInForest f ts

dropSubTree :: (a -> Bool) -> Tree a -> Tree a
dropSubTree _ (Node x [])  = Node x []
{-dropSubTree f (Node x sub) = -}


size :: Tree a -> Int
size = getSum . F.foldMap (const $ Sum 1)

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)


filter :: (a -> Bool) -> Tree a -> Tree a
filter p = foldTree f
    where f a ts = Node a (L.filter (p . rootLabel) ts)

-- | remove all subtrees whose nodes do not fulfill predicate
filterSub :: (a -> Bool) -> Tree a -> Tree a
filterSub f t = Node
                 (rootLabel t)
                 (map (filterSub f) $ L.filter (treeany f) $ subForest t)

-- | is predicate true in any node of tree ?
treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = f (rootLabel t) || any (treeany f) (subForest t)

mirror :: Tree a -> Tree a
mirror (Node a ts) = Node a . reverse $ map mirror ts
