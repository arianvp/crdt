{-#LANGUAGE MultiParamTypeClasses #-} 
{-#LANGUAGE FlexibleContexts #-}
module CRDT.IncrementOnlyCounter where
import Algebra.Lattice
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Control.Monad.Writer.Class


-- an Increment-only counter. Ord a is the replica index
newtype IncrementOnlyCounter a = IncrementOnlyCounter (Map.Map a Int)

instance Ord a => Show (IncrementOnlyCounter a) where
  show x = "IncrementOnlyCounter " ++ show (value x)

instance Ord a => JoinSemiLattice (IncrementOnlyCounter a) where
  (IncrementOnlyCounter v1) \/ (IncrementOnlyCounter v2) = 
    IncrementOnlyCounter (Map.unionWith max v1 v2)


instance Ord a => BoundedJoinSemiLattice (IncrementOnlyCounter a) where
  bottom = IncrementOnlyCounter Map.empty 

local :: Ord a => a -> IncrementOnlyCounter a -> Int
local i (IncrementOnlyCounter m) = m ! i

value :: Ord a => IncrementOnlyCounter a -> Int
value (IncrementOnlyCounter v) = Map.foldr (+) 0 v

increment :: Ord a => a -> IncrementOnlyCounter a -> IncrementOnlyCounter a
increment i (IncrementOnlyCounter v) =
  IncrementOnlyCounter (Map.insertWith (\a b -> a + b + 1) i 1 v)

incrementDelta :: Ord a => a -> IncrementOnlyCounter a -> IncrementOnlyCounter a
incrementDelta i (IncrementOnlyCounter m) = IncrementOnlyCounter (Map.singleton i (m ! i + 1))

increment' :: Ord a => a -> IncrementOnlyCounter a -> IncrementOnlyCounter a
increment' i x = incrementDelta i x \/ x


