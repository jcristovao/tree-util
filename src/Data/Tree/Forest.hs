{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree.Forest
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


module Data.Tree.Forest
  (
    Tree(..), Forest
  , drawForest
  , unfoldForest
  , unfoldForestM
  , unfoldForestM_BF
  , lookupTreeInForest
  , lookupTreeInForestBy
  ) where

import Data.Tree.Internal



-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest String -> String
drawForest  = unlines . map drawTree


