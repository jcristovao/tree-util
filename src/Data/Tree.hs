{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Multi-way trees (/aka/ rose trees) and forests.
--
-----------------------------------------------------------------------------


module Data.Tree
  (
    Tree(..), Forest
  , drawTree
  , draw
  , flatten
  , subTrees
  , levels
  , subTreesByLevel
  , unfoldTree
  , unfoldTreeM
  , unfoldTreeM_BF
  , lookupTree
  , lookupTreeBy
  , filterPruneTree
  , filterGraftTree
  , size
  , maxDepth
  , prune
  , mirror
  , cojoin
  ) where

import Prelude hiding (filter)
import Data.Tree.Internal
import qualified Data.Foldable as F
import Data.Monoid

-- | The elements of a tree in pre-order.
flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Node x ts) xs = x:Prelude.foldr squish xs ts

-- | List of subtrees (including the tree itself), in pre-order.
subTrees :: Tree a -> [Tree a]
subTrees t = squish t []
  where squish tr@(Node _ ts) xs = tr:Prelude.foldr squish xs ts


-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

-- | List of subtrees at each level of the tree.
subTreesByLevel :: Tree a -> [[Tree a]]
subTreesByLevel t = takeWhile (not . null) $
                    iterate (concatMap subForest) [t]

size :: Tree a -> Int
size = getSum . F.foldMap (const $ Sum 1)

-- https://stackoverflow.com/questions/21205213/haskell-tail-recursion-version-of-depth-of-binary-tree
maxDepth :: Tree a -> Int
maxDepth (Node _ []) = 1
maxDepth (Node _ fr) = 1 + maximum (map maxDepth fr)

mirror :: Tree a -> Tree a
mirror (Node a ts) = Node a . reverse $ map mirror ts

-- Ross Paterson suggestions -------------------------------------------------

-- | Label each node of the tree with its full subtree.
cojoin :: Tree a -> Tree (Tree a)
cojoin t@(Node _ ts) = Node t (map cojoin ts)

-- | remove all nodes past a certain depth
prune :: Int -> Tree a -> Tree a
prune 0 t = Node (rootLabel t) []
prune d t = Node (rootLabel t) (map (prune $ d-1) $ subForest t)

-- My stupid functions -----------------------------------------------------

dropSubTree :: (a -> Bool) -> Tree a -> Tree a
dropSubTree _ (Node x [])  = Node x []
{-dropSubTree f (Node x sub) = -}

-- |
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)


