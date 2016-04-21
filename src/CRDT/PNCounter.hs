module CRDT.PNCounter where
import Algebra.Lattice
import qualified CRDT.IncrementOnlyCounter as GCounter


data PNCounter = PNCounter
  { p :: GCounter.IncrementOnlyCounter
  , n :: GCounter.IncrementOnlyCounter
  }


instance JoinSemiLattice PNCounter where
  (PNCounter p1 n1) \/ (PNCounter p2 n2) = PNCounter (p1 \/ p2) (n1 \/ n2)
    
instance BoundedJoinSemiLattice PNCounter where
  bottom = PNCounter bottom bottom


local :: Int -> PNCounter -> Int
local i (PNCounter p n) =
  GCounter.local i p - GCounter.local i n


value :: PNCounter -> Int
value (PNCounter p n) = GCounter.value p - GCounter.value n

increment :: Int -> PNCounter -> PNCounter
increment i (PNCounter p n) = PNCounter (GCounter.increment i p) n

incrementWithDelta :: Int -> PNCounter -> (PNCounter, PNCounter)
incrementWithDelta i pn =
  let newValue = increment i pn
  in (newValue, bottom { p = p newValue })


decrement :: Int -> PNCounter -> PNCounter
decrement i (PNCounter p n) = PNCounter p (GCounter.increment i n)

decrementWithDelta :: Int -> PNCounter -> (PNCounter, PNCounter)
decrementWithDelta i pn =
  let newValue = decrement i pn
  in (newValue, bottom { n = n newValue })

