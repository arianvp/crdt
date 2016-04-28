{-#LANGUAGE FlexibleContexts #-}
module CRDT.AntiEntropy where
import Algebra.Lattice
import Control.Monad.Writer.Class
import Control.Monad.Random.Class




-- Chooses to return a delta or the full state
choose' :: ( BoundedJoinSemiLattice w
           , MonadWriter (Join w) m
           ) =>  w  -> Bool -> m w
choose' w0 x = do
  w <-
    if x
      then do 
        (_,Join w) <- listen (return ())
        return w
      else return w0
  censor (const mempty) (return w)
  

choose :: ( MonadRandom m
          , BoundedJoinSemiLattice w
          , MonadWriter (Join w) m
          ) => w -> m w
choose w = getRandom >>= choose' w

{-
antiEntropyCtr :: ( MonadRandom m
                  , BoundedJoinSemiLattice w
                  , MonadWriter m (Join w))
                  ) => 
-}
