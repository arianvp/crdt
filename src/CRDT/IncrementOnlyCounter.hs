{-#LANGUAGE MultiParamTypeClasses #-} 
{-#LANGUAGE FlexibleContexts #-}
module CRDT.IncrementOnlyCounter where
import Algebra.Lattice
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Control.Monad.Writer.Class


-- The increment-only counter assumes that we have a fixed network size, and a unique id
-- within the counter.
--

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
increment myIndex (IncrementOnlyCounter v) =
  IncrementOnlyCounter (Map.insertWith (\a b -> a + b + 1) myIndex 1 v)

incrementWithDelta :: (Ord a, MonadWriter (IncrementOnlyCounter a) m) => a -> IncrementOnlyCounter a -> m (IncrementOnlyCounter a)
incrementWithDelta i c = do
  let newCounter = increment i c
  tell (IncrementOnlyCounter (Map.singleton i (local i newCounter)))
  return newCounter

{-incrementWithDelta i c =
  let newCounter = increment i c
  in (newCounter, IncrementOnlyCounter (Map.singleton i (local i newCounter)))

-}


