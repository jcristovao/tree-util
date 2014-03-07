{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Tree.Util
  (
    lookupTree
  , lookupTreeInForest
  , lookupTreeBy
  , lookupTreeInForestBy
  , filter
  , filterPruneTree
  , filterSub
  , size
  , maxDepth
  , prune
  , mirror
  ) where

import Prelude hiding (filter)
import Data.Tree
import Data.Maybe
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

lookupTreeBy :: (a -> Bool) -> Tree a -> Maybe (Tree a)
lookupTreeBy f t
  | f (rootLabel t) = Just t
  | otherwise       = lookupTreeInForestBy f $ subForest t

lookupTreeInForestBy :: (a -> Bool) -> [Tree a] -> Maybe (Tree a)
lookupTreeInForestBy _ [] = Nothing
lookupTreeInForestBy f (t:ts) = case lookupTreeBy f t of
                             Just t' -> Just t'
                             Nothing -> lookupTreeInForestBy f ts

dropSubTree :: (a -> Bool) -> Tree a -> Tree a
dropSubTree _ (Node x [])  = Node x []
{-dropSubTree f (Node x sub) = -}


size :: Tree a -> Int
size = getSum . F.foldMap (const $ Sum 1)

-- https://stackoverflow.com/questions/21205213/haskell-tail-recursion-version-of-depth-of-binary-tree
maxDepth :: Tree a -> Int
maxDepth (Node _ []) = 1
maxDepth (Node _ fr) = 1 + maximum (map maxDepth fr)


foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)


filter :: (a -> Bool) -> Tree a -> Tree a
filter p = foldTree f
    where f a ts = Node a (L.filter (p . rootLabel) ts)

-- | Prune every subtree whose root label does not match.
filterPruneTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filterPruneTree p (Node x ns)
   | p x = Just . Node x $ filterPruneForest p ns
   | otherwise = Nothing

filterPruneForest :: (a -> Bool) -> Forest a -> Forest a
filterPruneForest = mapMaybe . filterPruneTree

-- | remove all subtrees whose nodes do not fulfill predicate
filterSub :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filterSub f (Node x ns)
  | f x = Just.  Node x $
                 (mapMaybe (filterSub f) $ L.filter (treeany f) ns)
  | otherwise = Nothing

-- | is predicate true in any node of tree ?
treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = f (rootLabel t) || any (treeany f) (subForest t)

mirror :: Tree a -> Tree a
mirror (Node a ts) = Node a . reverse $ map mirror ts

-- | remove all nodes past a certain depth
prune :: Int -> Tree a -> Tree a
prune 0 t = Node (rootLabel t) []
prune d t = Node (rootLabel t) (map (prune $ d-1) $ subForest t)
