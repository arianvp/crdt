{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module CRDT.GrowOnlySet
( GrowOnlySet
, insert
, member
) where

import qualified Data.Set as Set
import Algebra.Lattice

newtype GrowOnlySet a = GrowOnlySet (Set.Set a)
  deriving (JoinSemiLattice, BoundedJoinSemiLattice, Show)

-- query
member :: Ord a => a -> GrowOnlySet a -> Bool
member a (GrowOnlySet s) = Set.member a s

-- update
insert :: Ord a => a -> GrowOnlySet a -> GrowOnlySet a
insert a (GrowOnlySet s) = GrowOnlySet (Set.insert a s)

-- Return
insertDelta :: Ord a => a -> GrowOnlySet a -> GrowOnlySet a
insertDelta a (GrowOnlySet s) = GrowOnlySet (Set.singleton a)
