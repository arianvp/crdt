module CRDT.IncrementOnlyCounter where
import Algebra.Lattice
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable


-- The increment-only counter assumes that we have a fixed network size, and a unique id
-- within the counter.
--

newtype IncrementOnlyCounter = IncrementOnlyCounter (Map.Map Int Int)

instance Show IncrementOnlyCounter where
  show x = "IncrementOnlyCounter " ++ show (value x)


increment :: Int -> IncrementOnlyCounter -> IncrementOnlyCounter
increment myIndex (IncrementOnlyCounter v) =
  IncrementOnlyCounter (Map.insertWith (\a b -> a + b + 1) myIndex 1 v)

value :: IncrementOnlyCounter -> Int
value (IncrementOnlyCounter v) = Map.foldr (+) 0 v


instance JoinSemiLattice IncrementOnlyCounter where
  (IncrementOnlyCounter v1) \/ (IncrementOnlyCounter v2) = 
    IncrementOnlyCounter (Map.unionWith max v1 v2)


instance BoundedJoinSemiLattice IncrementOnlyCounter where
  bottom = IncrementOnlyCounter Map.empty 



-- Delta-mutator based Increment-only counter



