module CRDT.IncrementOnlyCounter where
import Algebra.Lattice
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable


-- The increment-only counter assumes that we have a fixed network size, and a unique id
-- within the counter.
--

newtype IncrementOnlyCounter = IncrementOnlyCounter (Map.Map Int Int)

instance Show IncrementOnlyCounter where
  show x = "IncrementOnlyCounter " ++ show (value x)

instance JoinSemiLattice IncrementOnlyCounter where
  (IncrementOnlyCounter v1) \/ (IncrementOnlyCounter v2) = 
    IncrementOnlyCounter (Map.unionWith max v1 v2)


instance BoundedJoinSemiLattice IncrementOnlyCounter where
  bottom = IncrementOnlyCounter Map.empty 


local :: Int -> IncrementOnlyCounter -> Int
local i (IncrementOnlyCounter m) = m ! i

value :: IncrementOnlyCounter -> Int
value (IncrementOnlyCounter v) = Map.foldr (+) 0 v

increment :: Int -> IncrementOnlyCounter -> IncrementOnlyCounter
increment myIndex (IncrementOnlyCounter v) =
  IncrementOnlyCounter (Map.insertWith (\a b -> a + b + 1) myIndex 1 v)


-- TODO: use state monad
incrementWithDelta :: Int -> IncrementOnlyCounter -> (IncrementOnlyCounter, IncrementOnlyCounter)

incrementWithDelta i c =
  let newCounter = increment i c
  in (newCounter, IncrementOnlyCounter (Map.singleton i (local i newCounter)))



