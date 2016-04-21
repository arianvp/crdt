module CRDT.IncrementOnlyCounter where
import Algebra.Lattice
import qualified Data.Vector as Vector
import Data.Vector ((!), (//))


-- The increment-only counter assumes that we have a fixed network size, and a unique id
-- within the counter.
--

newtype IncrementOnlyCounter = IncrementOnlyCounter
  { counters :: Vector.Vector Int  -- ^contains all counters in the network
  }


increment :: Int -> IncrementOnlyCounter -> IncrementOnlyCounter
increment myIndex (IncrementOnlyCounter counters) = IncrementOnlyCounter (counters // [(myIndex, (counters ! myIndex) + 1)])

value :: IncrementOnlyCounter -> Int
value c = Vector.sum (counters c)


instance JoinSemiLattice IncrementOnlyCounter where
  (IncrementOnlyCounter v1) \/ (IncrementOnlyCounter v2) = 
    IncrementOnlyCounter (Vector.zipWith max v1 v2)

instance BoundedJoinSemiLattice IncrementOnlyCounter where
  bottom = IncrementOnlyCounter (Vector.replicate 255 0) -- A subnet is max 255 in size
  






