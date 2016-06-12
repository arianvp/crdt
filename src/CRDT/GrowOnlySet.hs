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

-- | Checks if an element is in the Set
member :: Ord a => a -> GrowOnlySet a -> Bool
member a (GrowOnlySet s) = Set.member a s

-- | Inserts an element in the set in an eventually consistent manner
insert :: Ord a => a -> GrowOnlySet a -> GrowOnlySet a
insert a (GrowOnlySet s) = GrowOnlySet (Set.insert a s)

-- | Returns a delta that represents an insert,  joining this with
--  a set inserts the element.  Joining this with other insertions
--  creates a larger insertion operation.
insertDelta :: Ord a => a -> GrowOnlySet a -> GrowOnlySet a
insertDelta a (GrowOnlySet s) = GrowOnlySet (Set.singleton a)
