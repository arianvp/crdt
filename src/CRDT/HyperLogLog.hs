{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE UndecidableInstances #-}
module CRDT.HyperLogLog where
import Data.HyperLogLog
import Data.Reflection
import Algebra.Lattice
import Data.Monoid



instance (Reifies a Integer) => JoinSemiLattice (HyperLogLog a) where
  a  \/ b = a <> b

instance (Reifies a Integer) => BoundedJoinSemiLattice (HyperLogLog a) where
  bottom = mempty



