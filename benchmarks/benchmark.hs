module Main where

import Data.Tree
import Data.Tree.Forest
import Criterion.Main

rosTree :: Tree Int
rosTree = Node 1 [ Node 2 [ Node 4 [Node 7 [ Node 10 [ Node 15 []
                                                     , Node 13 []
                                                     ]
                                           ]
                                   ]
                          , Node 5 [ Node 12 [ Node 3 []
                                             , Node 2 []
                                             , Node 14 []
                                             ]
                                   ]
                          , Node 6 [ Node 1  [ Node 9 [ Node 14 [ Node 13 []
                                                                , Node  9 []
                                                                ]
                                                      ]
                                             , Node 11 [ Node 2 []]
                                             ]
                                    ]
                          ]
                 , Node 3 [ Node 6 [ Node 8 []
                          , Node 9 []]]
                 , Node 5 [ Node 7 []
                          , Node 11 [Node 19 []
                          , Node  8 [Node 1  []
                          , Node 16 []]]
                          ]
                 , Node 6 [ Node 1  []
                          , Node  1 [ Node 10 []
                                    , Node 7 [ Node 7 []
                                             , Node 17[ Node 0 [ Node 9 [ Node 8 []
                                                                        , Node 7 []
                                                                        , Node 5 []
                                                                        , Node 1 []
                                                                        ]
                                                               , Node 4 []
                                                               , Node 3 [ Node 2 []
                                                                        , Node 1 []
                                                                        ]
                                                               ]
                                                       , Node 1 []
                                                       ]
                                             ]
                                    ]
                          ]
                 , Node 8 [ Node 3 []
                          , Node 18 [ Node 15 []
                                    , Node 9 [ Node 9 []
                                             , Node 11[]]
                                    ]
                          ]
                 ]


main :: IO ()
main = defaultMain
    [ bench "flatten"           $ nf flatten            rosTree
    , bench "flattenPostOrder"  $ nf flattenPostOrder   rosTree
    , bench "mirror"            $ nf mirror             rosTree
    , bench "subTrees"          $ nf subTrees           rosTree
    , bench "flatten . cojoin"  $ nf (flatten . cojoin) rosTree
    , bench "subTreesByLevel"   $ nf subTreesByLevel    rosTree
    , bench "levels . cojoin"   $ nf (levels . cojoin)  rosTree
    ]

