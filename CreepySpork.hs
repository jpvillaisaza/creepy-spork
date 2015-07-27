module CreepySpork where

import Control.Monad
import Test.QuickCheck

-- choose

dice :: Gen Int
dice =
  choose (1, 6)

arbitraryBool :: Gen Bool
arbitraryBool =
  choose (False, True)

-- oneof

arbitraryEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
arbitraryEither =
  oneof [ liftM Left arbitrary, liftM Right arbitrary ]

-- frequency

loadedDice :: Gen Int
loadedDice =
  frequency [ (1, choose (1, 5)), (1, return 6) ]

arbitraryMaybe :: Arbitrary a => Gen (Maybe a)
arbitraryMaybe =
  frequency [ (1, return Nothing), (3, liftM Just arbitrary) ]

-- elements

arbitraryOrdering :: Gen Ordering
arbitraryOrdering =
  elements [ LT, EQ, GT ]

-- sized

arbitraryList :: Arbitrary a => Gen [a]
arbitraryList =
  sized $ \n -> do
    k <- choose (0, n)
    sequence [ arbitrary | _ <- [1..k] ]

-- example

data Tree a
  = Tree a [Tree a]
  deriving (Show)

nodes :: Tree a -> Int
nodes (Tree _ ts) =
  1 + sum (fmap nodes ts)

edges :: Tree a -> Int
edges (Tree _ []) = 0
edges (Tree _ ts) =
  length ts + sum (fmap edges ts)

levels :: Tree a -> Int
levels (Tree _ []) = 1
levels (Tree _ ts) =
  1 + maximum (fmap levels ts)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree 0 = do
  t <- arbitrary
  return (Tree t [])
arbitrarySizedTree m = do
  t <- arbitrary
  n <- choose (1, m `div` 4)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 8))
  return (Tree t ts)
