{-# LANGUAGE ExistentialQuantification #-}
module CRDT.Fold where

import Algebra.Lattice
import Control.Applicative
import Data.List (foldl')
import Data.Monoid
import Data.Strict.Tuple
import Prelude hiding (sum, length)

-- Allows us to effeciently fold over bounded join semilattices
data Fold a b = forall w. (BoundedJoinSemiLattice w) => Fold
    { tally   :: a -> w
    , compute :: w -> b
    }

fold :: Fold a b -> [a] -> b
fold (Fold t c) xs =
    c (foldl' (\/) bottom (map t xs))

instance (JoinSemiLattice a, JoinSemiLattice b) => JoinSemiLattice (Pair a b) where
    (aL :!: aR) \/ (bL :!: bR) = aL \/ bL :!: aR \/ bR

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (Pair a b) where
    bottom = (bottom :!: bottom)

instance Functor (Fold a) where
    fmap f (Fold t k) = Fold t (f . k)

instance Applicative (Fold a) where
    pure a    = Fold (\_ -> ()) (\_ -> a)
    (Fold tL cL) <*> (Fold tR cR) =
        let t x = (tL x :!: tR x)
            c (wL :!: wR) = (cL wL) (cR wR)
        in  Fold t c

