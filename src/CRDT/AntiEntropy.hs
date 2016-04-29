{-#LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module CRDT.AntiEntropy where
import Algebra.Lattice
import Data.IORef
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

-- A JoinWriter is a very special version of the Writer monad because the underlying log
-- is backed by an IORef that can be modified by multiple writers running at the same time.
-- This might sound unsafe, but because of more constraints, the results are eventually
-- consistent.  Instead of restricting the log to a monoid, we restrict the log to a
-- bounded join-semilattice. These are also commutative and idempotent. So as long as we 
-- can guarentee that every JoinWriter eventually receives all messages, in any order,
-- then we know that we'll be eventually consistent.
data JoinWriter w a = JoinWriter { runJoinWriter :: IORef w -> IO a }


instance Functor (JoinWriter w) where
  fmap f (JoinWriter g) = JoinWriter (fmap f . g)

instance Applicative (JoinWriter w) where
  pure  = JoinWriter . const . pure
  JoinWriter f <*> JoinWriter g = JoinWriter (\w -> f w <*> g w)

instance Monad (JoinWriter w) where
  JoinWriter f >>= g = JoinWriter (\w -> f w >>= \x -> runJoinWriter (g x) w)


instance BoundedJoinSemiLattice w => MonadWriter (Join w) (JoinWriter (Join w)) where
  writer (a,w) =
    JoinWriter
      (\ref -> do
        modifyIORef ref (`mappend` w)
        return a)
  listen (JoinWriter f) =
    JoinWriter
      (\ref -> do
        w <- readIORef ref
        v <- f ref
        return (v,w))
  pass (JoinWriter f) =
    JoinWriter
      (\ref -> do
        (a, g) <- f ref
        modifyIORef ref g
        return a)

