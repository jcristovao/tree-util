{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TreeUtilSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Prelude hiding (length, filter)
import qualified Data.List as L
import Data.Tree
import Data.Tree.Util


{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
{-# ANN spec ("HLint: ignore Use mappend"::String) #-}
spec :: Spec
spec = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "Data.Tree.Util" $ do
    let bigTree, tree5, tree6 :: Tree Int
        bigTree = Node 0 [ Node 2 [ Node 5 [Node 6 []]
                                  , Node 4 [Node 3 []]]
                         , Node 1 [Node 5 []]
                         ]
        tree5   = Node 5 []
        tree6   = Node 6 []

    it "mirror" $ do
      mirror bigTree `shouldBe` Node 0 [ Node 1 [ Node 5 []]
                                       , Node 2 [ Node 4 [ Node 3 []]
                                                , Node 5 [ Node 6 []]
                                                ]
                                       ]

    it "lookupTreeBy" $ do
      lookupTreeBy (==5) tree5   `shouldBe` Just (Node 5 [])
      lookupTreeBy (==5) tree6   `shouldBe` Nothing
      lookupTreeBy (==5) bigTree `shouldBe` Just (Node 5 [Node 6 []])

    it "find result size is <= original tree size" . property
      $ \(tr :: Tree Int) -> size tr >= (maybe 0 size . lookupTreeBy (== 0) $ tr)

    it "find result is a subset of original tree" . property
      $ \(tr :: Tree Int) -> let f = lookupTreeBy (== 0) tr
                             in case f of
                               Nothing -> True
                               Just x  -> flatten x `L.isInfixOf` flatten tr
    it "lookupTree" $ do
      lookupTree 5 tree5   `shouldBe` Just (Node 5 [])
      lookupTree 5 tree6   `shouldBe` Nothing
      lookupTree 5 bigTree `shouldBe` Just (Node 5 [Node 6 []])

    it "lookupTree result size is <= original tree size" . property
      $ \(tr :: Tree Int) -> size tr >= (maybe 0 size . lookupTree 0 $ tr)

    it "lookupTree result is a subset of original tree" . property
      $ \(tr :: Tree Int) -> let f = lookupTree 0 tr
                             in case f of
                               Nothing -> True
                               Just x  -> flatten x `L.isInfixOf` flatten tr

    it "lookupTree vs lookupTreeBy" . property
      $ \(tr :: Tree Int) -> lookupTree 0 tr == lookupTreeBy (==0) tr

    {-it "filter" $ do-}
      {-filter (== 0) bigTree `shouldBe` Node 0 []-}
      {-filter (/= 2) bigTree `shouldBe` Node 0 [Node 1 [Node 5 []]]-}
      {-filter (== 1) bigTree `shouldBe` Node 0 [Node 1 []]-}
      {-filter (== 9) bigTree `shouldBe` Node 0 []-}
      {-filter (== 5) bigTree `shouldBe` Node 0 [ Node 1 [ Node 5 []]]-}
      {-filter (== 3) bigTree `shouldBe` Node 0 [ Node 2 [ Node 4 [ Node 3 []]]]-}
      {-filter (== 1) wrongTree `shouldBe` wrongTree-}

    {-it "filterSub" $ do-}
      {-filterSub (== 0) bigTree `shouldBe` Just (Node 0 [])-}
      {-filterSub (/= 2) bigTree `shouldBe` Just (Node 0 [Node 1 [Node 5 []]])-}
      {-filterSub (== 1) bigTree `shouldBe` Nothing -- Node 0 [Node 1 []]-}

    {-it "filterSub vs filterPruneTree" . property-}
      {-$ \(tr :: Tree Int) -> filterSub (==0) tr == filterPruneTree (== 0) tr-}

    it "size" . property
      $ \(tr :: Tree Int) -> size tr == (L.length . flatten $ tr)

    it "maxDepth" $ do
      maxDepth bigTree `shouldBe` 4
      maxDepth tree5   `shouldBe` 1

    it "prune" . property $
      \(tr :: Tree Int,mx :: Positive Int) -> maxDepth (prune (getPositive mx) tr)
                                              <= getPositive mx + 1
