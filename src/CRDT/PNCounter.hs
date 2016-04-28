module CRDT.PNCounter where
import Algebra.Lattice
import qualified CRDT.IncrementOnlyCounter as GCounter


data PNCounter a = PNCounter
  { p :: GCounter.IncrementOnlyCounter a 
  , n :: GCounter.IncrementOnlyCounter a
  }

instance Ord a => Show (PNCounter a) where
  show x = "PNCounter " ++ show (value x)

instance Ord a => JoinSemiLattice (PNCounter a) where
  (PNCounter p1 n1) \/ (PNCounter p2 n2) = PNCounter (p1 \/ p2) (n1 \/ n2)
    
instance Ord a => BoundedJoinSemiLattice (PNCounter a) where
  bottom = PNCounter bottom bottom


local :: Ord a => a -> PNCounter a -> Int
local i (PNCounter p n) =
  GCounter.local i p - GCounter.local i n


value :: Ord a => PNCounter a -> Int
value (PNCounter p n) = GCounter.value p - GCounter.value n

increment :: Ord a => a -> PNCounter a -> PNCounter a
increment i (PNCounter p n) = PNCounter (GCounter.increment i p) n

incrementWithDelta :: Ord a => a -> PNCounter a -> (PNCounter a , PNCounter a)
incrementWithDelta i pn =
  let newValue = increment i pn
  in (newValue, bottom { p = p newValue })


decrement :: Ord a => a -> PNCounter a  -> PNCounter a
decrement i (PNCounter p n) = PNCounter p (GCounter.increment i n)

decrementWithDelta :: Ord a => a -> PNCounter a -> (PNCounter a, PNCounter a)
decrementWithDelta i pn =
  let newValue = decrement i pn
  in (newValue, bottom { n = n newValue })

