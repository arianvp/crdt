module CRDT.TwoPSet where

import qualified Data.Set as Set
import Algebra.Lattice

data TwoPSet a = TwoPSet (Set.Set a) (Set.Set a)
  deriving Show


instance Ord a => JoinSemiLattice (TwoPSet a) where
  (TwoPSet s1 t1) \/ (TwoPSet s2 t2) = --(s1 /\ t2)  \/  (s2 /\ t1)
    TwoPSet s t
    where
      -- add tombstones together
      t = t1 \/ t2
      -- delete elements in s1 that are tombstoned in t2
      -- add alements from s2 that weren't tombstoned in t1
      s = (s1 /\ t2) \/ (s2 /\ t1)


instance Ord a => BoundedJoinSemiLattice (TwoPSet a) where
  bottom = TwoPSet bottom bottom

insert :: Ord a => a -> TwoPSet a -> TwoPSet a
insert a tp@(TwoPSet s t) =
  if Set.member a t
    then tp
    else TwoPSet (Set.insert a s) t

insertDelta :: Ord a => a -> TwoPSet a -> TwoPSet a
insertDelta a tp@(TwoPSet s t) =
  if Set.member a t
    then insert a bottom
    else bottom

delete :: Ord a => a -> TwoPSet a -> TwoPSet a
delete a tp@(TwoPSet s t) =
  TwoPSet (Set.delete a s) (Set.insert a t)

deleteDelta :: Ord a => a -> TwoPSet a -> TwoPSet a
deleteDelta a tp@(TwoPSet s t) =
  TwoPSet bottom (Set.insert a t)




