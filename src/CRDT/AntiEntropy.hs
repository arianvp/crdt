{-#LANGUAGE FlexibleContexts #-}
module CRDT.AntiEntropy where
import Algebra.Lattice
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.Random.Class




-- Chooses to return a delta or the full state
choose :: ( MonadRandom m
          , BoundedJoinSemiLattice w
          , MonadState w m
          , MonadWriter (Join w) m
          ) => m w
choose = do
  x <- getRandom
  w <-
    if x
      then do 
        (_,Join w) <- listen (return ())
        return w
      else get
  censor (const mempty) (return w)
      

{-
antiEntropyCtr :: ( MonadRandom m
                  , BoundedJoinSemiLattice w
                  , MonadWriter m (Join w))
                  ) => 
-}
