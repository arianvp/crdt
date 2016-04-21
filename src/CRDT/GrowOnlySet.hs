{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module CRDT.GrowOnlySet
( GrowOnlySet
) where

import qualified Data.Set as Set
import Algebra.Lattice

newtype GrowOnlySet a = GrowOnlySet (Set.Set a)
  deriving (JoinSemiLattice, BoundedJoinSemiLattice)

-- update
insert :: Ord a => a -> GrowOnlySet a -> GrowOnlySet a
insert a (GrowOnlySet s) = GrowOnlySet (Set.insert a s)

-- query
member :: Ord a => a -> GrowOnlySet a -> Bool
member a (GrowOnlySet s) = Set.member a s
