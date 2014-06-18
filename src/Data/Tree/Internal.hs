{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.Internal
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

module Data.Tree.Internal(
    Tree(..), Forest,
    -- * Two-dimensional drawing
    draw, drawTree,
    -- * Building trees
    unfoldTree, unfoldForest,
    unfoldTreeM, unfoldForestM,
    unfoldTreeM_BF, unfoldForestM_BF,
    lookupTree, lookupTreeBy, lookupTreeInForest, lookupTreeInForestBy,
    filterPruneTree, filterPruneForest
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (liftM)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
            ViewL(..), ViewR(..), viewl, viewr)
import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))
import Data.Typeable
import Control.DeepSeq (NFData(rnf))

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
#endif

-- | Multi-way trees, also known as /rose trees/.
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq, Read, Show, Data)
#else
  deriving (Eq, Read, Show)
#endif
type Forest a = [Tree a]

#include "Typeable.h"
INSTANCE_TYPEABLE1(Tree,treeTc,"Tree")

instance Functor Tree where
    fmap f (Node x ts) = Node (f x) (map (fmap f) ts)

instance Applicative Tree where
    pure x = Node x []
    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

instance Monad Tree where
    return x = Node x []
    Node x ts >>= f = Node x' (ts' ++ map (>>= f) ts)
      where Node x' ts' = f x

instance Traversable Tree where
    traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
    foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts

instance NFData a => NFData (Tree a) where
    rnf (Node x ts) = rnf x `seq` rnf ts

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | Build a tree from a seed value
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
unfoldForest f = map (unfoldTree f)

-- | Monadic tree builder, in depth-first order
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM f b = do
    (a, bs) <- f b
    ts <- unfoldForestM f bs
    return (Node a ts)

-- | Monadic forest builder, in depth-first order
#ifndef __NHC__
unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
#endif
unfoldForestM f = Prelude.mapM (unfoldTreeM f)

-- | Monadic tree builder, in breadth-first order,
-- using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF f b = liftM getElement $ unfoldForestQ f (singleton b)
  where
    getElement xs = case viewl xs of
        x :< _ -> x
        EmptyL -> error "unfoldTreeM_BF"

-- | Monadic forest builder, in breadth-first order,
-- using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldForestM_BF :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForestM_BF f = liftM toList . unfoldForestQ f . fromList

-- takes a sequence (queue) of seeds
-- produces a sequence (reversed queue) of trees of the same length
unfoldForestQ :: Monad m => (b -> m (a, [b])) -> Seq b -> m (Seq (Tree a))
unfoldForestQ f aQ = case viewl aQ of
    EmptyL -> return empty
    a :< aQ' -> do
        (b, as) <- f a
        tQ <- unfoldForestQ f (Prelude.foldl (|>) aQ' as)
        let (tQ', ts) = splitOnto [] as tQ
        return (Node b ts <| tQ')
  where
    splitOnto :: [a'] -> [b'] -> Seq a' -> (Seq a', [a'])
    splitOnto as [] q = (q, as)
    splitOnto as (_:bs) q = case viewr q of
        q' :> a -> splitOnto (a:as) bs q'
        EmptyR -> error "unfoldForestQ"

-- | Prune every subtree whose root label does not match.
filterPruneTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filterPruneTree p (Node x ns)
   | p x = Just . Node x $ filterPruneForest p ns
   | otherwise = Nothing

filterPruneForest :: (a -> Bool) -> Forest a -> Forest a
filterPruneForest = mapMaybe . filterPruneTree

-- | get the sub-tree rooted at the first (left-most, depth-first) occurrence
-- of the specified node value
lookupTree :: Eq a => a -> Tree a -> Maybe (Tree a)
lookupTree v t
    | rootLabel t == v = Just t
    | otherwise = lookupTreeInForest v $ subForest t

lookupTreeBy :: (a -> Bool) -> Tree a -> Maybe (Tree a)
lookupTreeBy f t
  | f (rootLabel t) = Just t
  | otherwise       = lookupTreeInForestBy f $ subForest t

-- | get the sub-tree for the specified node value in the first tree in
-- forest in which it occurs.
lookupTreeInForest :: Eq a => a -> [Tree a] -> Maybe (Tree a)
lookupTreeInForest _ [] = Nothing
lookupTreeInForest v (t:ts) = case lookupTree v t of
                             Just t' -> Just t'
                             Nothing -> lookupTreeInForest v ts

lookupTreeInForestBy :: (a -> Bool) -> [Tree a] -> Maybe (Tree a)
lookupTreeInForestBy _ [] = Nothing
lookupTreeInForestBy f (t:ts) = case lookupTreeBy f t of
                             Just t' -> Just t'
                             Nothing -> lookupTreeInForestBy f ts
