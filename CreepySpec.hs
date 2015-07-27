module Main (main) where

import CreepySpork
import Test.QuickCheck

prop_OneMoreNodeThanEdges :: Tree Int -> Bool
prop_OneMoreNodeThanEdges tree =
  nodes tree == edges tree + 1

main :: IO ()
main =
  quickCheck prop_OneMoreNodeThanEdges
